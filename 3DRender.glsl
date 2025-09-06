#version 300 es

precision mediump float;

#define PI 3.14159265359
#define MAX_VERTICES_NUMBER 40
#define MAX_TRIANGLES_NUMBER 80 
#define MAX_ENTITY_NUMBER 5 
#define MAX_LIGHT_NUMBER 5 

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

// Fragment shader output
out vec4 fragColor;

/* ====================== UTILS ====================== */

// Calculate barycentric coordinates for the current fragment position (st)
vec3 barycentricCoords(vec2 p, vec2 a, vec2 b, vec2 c) {
    vec2 v0 = b - a;
    vec2 v1 = c - a;
    vec2 v2 = p - a;

    float denom = v0.x * v1.y - v1.x * v0.y; // Cross product for the full triangle area
    float lambda1 = (v2.x * v1.y - v1.x * v2.y) / denom; // Area(PBC) / Area(ABC)
    float lambda2 = (v0.x * v2.y - v2.x * v0.y) / denom; // Area(PCA) / Area(ABC)
    float lambda3 = 1.0 - lambda1 - lambda2;

    return vec3(lambda1, lambda2, lambda3);
}

vec3 getTriangleNormal(vec3 a, vec3 b, vec3 c) {
    vec3 edge1 = b - a;
    vec3 edge2 = c - a;
    vec3 normal = cross(edge1, edge2);
    return normalize(normal);
}

mat4 getViewMatrix(vec3 camPos, vec3 target, vec3 up) {
    vec3 zAxis = normalize(camPos - target);  // Forward
    vec3 xAxis = normalize(cross(up, zAxis)); // Right
    vec3 yAxis = cross(zAxis, xAxis);  // Up

    return mat4(
        vec4(xAxis.x, xAxis.y, xAxis.z , 0.0), //TODO : - added to fix the mirroring
        vec4(yAxis.x, yAxis.y, yAxis.z , 0.0),
        vec4(zAxis.x, zAxis.y, zAxis.z, 0.0),
        vec4(-dot(xAxis, camPos), -dot(yAxis, camPos), -dot(zAxis, camPos), 1.0)
    );
}

mat4 getPerspectiveMatrix(float fov, float aspect, float near, float far) {
    float f = 1.0 / tan(fov / 2.0);
    return mat4(
        vec4(f / aspect, 0.0, 0.0, 0.0),
        vec4(0.0, f, 0.0, 0.0),
        vec4(0.0, 0.0, (far + near) / (near - far), -1.0),
        vec4(0.0, 0.0, (2.0 * far * near) / (near - far), 0.0)
    );
}

/* ====================== STRUCTS ====================== */

struct Pixel{
    vec4 color; 
    float depth;
}; 

Pixel pixel;

struct Light{
    vec3 position;
    vec3 color; 
    float intensity;
};

struct Triangle {
    float visibility; 

    vec3 a; vec3 b; vec3 c;

    vec4 aW; vec4 bW; vec4 cW;
    vec3 normalW;

    vec3 aV; vec3 bV; vec3 cV;
    vec3 normalV;

    vec4 aC; vec4 bC; vec4 cC;
    vec3 ndc;
};

struct Material{
    vec4 color;
    float reflection;
    float transparency;
};

struct Mesh {

    vec3 vertices[MAX_VERTICES_NUMBER];
    int verticesLength;
    int triangles[MAX_TRIANGLES_NUMBER];
    int trianglesLength;
};

struct Transform{
    vec3 pos;
    mat3 rotation;
    mat3 scale;
};

struct Entity{
    Mesh mesh; 
    Material m;
    Transform transform;
};

struct Camera{
    vec3 position;
    vec3 target;
    vec3 up;
    
    mat4 viewMatrix;
    mat4 projectionMatrix;
};

/* ====================== TRANSFORM ====================== */

Transform default_transform = Transform(vec3(0.0), mat3(1.0), mat3(1.0));

void rotateX(inout Transform t, float a){
    t.rotation = t.rotation * mat3(1.0,0.0,0.0,  0.0,cos(a),-sin(a),  0.0,sin(a),cos(a));
} 

void rotateZ(inout Transform t, float a){
    t.rotation = t.rotation * mat3(cos(a),-sin(a),0.0,  sin(a),cos(a),0.0,  0.0,0.0,1.0);
}

void rotateY(inout Transform t, float a){
    t.rotation = t.rotation * mat3(cos(a),0.0,sin(a),  0.0,1.0,0.0,  -sin(a),0.0,cos(a));
}

void scale(inout Transform t, vec3 s) {
    // Matrice de scale diagonale
    mat3 scaleMatrix = mat3(
        s.x, 0.0, 0.0,
        0.0, s.y, 0.0,
        0.0, 0.0, s.z
    );

    t.scale = t.scale * scaleMatrix;
}

void scaleUniform(inout Transform t, float s) {
    scale(t, vec3(s, s, s));
}

mat4 modelMatrix(Transform t){
    mat3 rs = t.rotation * t.scale;
    return mat4(
        vec4(rs[0][0], rs[1][0], rs[2][0], 0.0),
        vec4(rs[0][1], rs[1][1], rs[2][1], 0.0),
        vec4(rs[0][2], rs[1][2], rs[2][2], 0.0),
        vec4(t.pos, 1.0)
    );
}

/* ====================== WORLD ====================== */

struct World{
    Camera camera;
    
    Entity entities[MAX_ENTITY_NUMBER];
    int entitiesLen;
    Light lights[MAX_LIGHT_NUMBER]; 
    int lightsLen;
    
    vec4 skyboxColor;
};

World world;

void AddEntity(inout World w, Entity e){
    w.entities[w.entitiesLen] = e;
    w.entitiesLen ++;
}

void AddLight(inout World w, Light l){
    w.lights[w.lightsLen] = l;
    w.lightsLen ++;
}

/* ====================== PIPELINES ====================== */

// Triangle Culling (Backface culling)
bool isTriangleVisible(Triangle t, Camera cam) {
    vec3 triCenter = (t.aV + t.bV + t.cV) / 3.0;
    // en VSpace la cam est tjr a (0,0,0)
    vec3 camToTri = normalize(triCenter - vec3(0.0));
    // Check si la normal va vers la cam 
    return dot(normalize(t.normalV), camToTri) < 0.0;
}

void GeometryPipeline(inout Triangle t, vec2 st, mat4 M , mat4 V, mat4 P){
    
    // Object => World 
    t.aW = M * vec4(t.a, 1.0);
    t.bW = M * vec4(t.b, 1.0);
    t.cW = M * vec4(t.c, 1.0);

    // Multiply by ViewMatrix
    vec4 aV4 = V * t.aW; vec4 bV4 = V * t.bW; vec4 cV4 = V * t.cW;
    t.aV = aV4.xyz / aV4.w; t.bV = bV4.xyz / bV4.w; t.cV = cV4.xyz / cV4.w;
    
    // Normal in view space
    mat3 normalMat = mat3(transpose(inverse(V * M)));
    t.normalV = normalize(normalMat * getTriangleNormal(t.a, t.b, t.c));
    
    // NDC -> [0,1] screen for barycentrics
    t.aC = P * aV4; t.bC = P * bV4; t.cC = P * cV4;
    vec2 aN = t.aC.xy / t.aC.w * 0.5 + 0.5;
    vec2 bN = t.bC.xy / t.bC.w * 0.5 + 0.5;
    vec2 cN = t.cC.xy / t.cC.w * 0.5 + 0.5;

    t.ndc = barycentricCoords(st, aN, bN, cN);

    t.visibility = isTriangleVisible(t, world.camera) ? step(0.0, min(t.ndc.x, min(t.ndc.y, t.ndc.z))) : 0.0;
}

vec4 ColorPipeline(inout Triangle t, Material mat, mat4 V){
    vec3 fragmentPosition = t.ndc.x * t.bV + t.ndc.y * t.cV + t.ndc.z * t.aV;
    
    vec4 fragColor = vec4(0.);

    for(int i = 0; i < MAX_LIGHT_NUMBER; i++){
        if(i >= world.lightsLen) { break; }

        vec3 lightPosView = (V * vec4(world.lights[i].position, 1.0)).xyz;

        vec3 lightDir = normalize(lightPosView - fragmentPosition);

        // light  Culling 
        float diffuseIntensity = max(dot(normalize(t.normalV), lightDir), 0.0);

        // Attenuation 
        float distance = length(lightPosView - fragmentPosition);
        float attenuation = 1.0 / (distance * distance);

        // Combine lighting
        vec3 baseLighting = diffuseIntensity * world.lights[i].intensity * attenuation * world.lights[i].color;
        vec3 finalColor = baseLighting * mat.color.rgb + (attenuation * mat.reflection * world.lights[i].color);

        fragColor += vec4(finalColor, 1.0 - mat.transparency);
    } 
    
    return fragColor;
}

/* ====================== RENDER ====================== */

vec4 renderTriangle(vec2 st, inout Triangle tr, Material mat, mat4 modelMatrix){
    
    GeometryPipeline(tr, st,
        modelMatrix,
        world.camera.viewMatrix,
        world.camera.projectionMatrix);
    
    if (tr.visibility > 0.){
        return ColorPipeline(tr, mat, world.camera.viewMatrix);
    }
    return vec4(0.); 
}

void renderEntity(vec2 st, Entity e) {
    if(e.mesh.trianglesLength <= 0) {
        pixel.color = vec4(0.);
        return ;
    }

    vec4 color = vec4(0.);
    mat4 modelMatrix = modelMatrix(e.transform);

    for(int i = 0; i < MAX_TRIANGLES_NUMBER; i+=3){
        if(i >= e.mesh.trianglesLength) { break; }
        
        Triangle t;
        t.a = e.mesh.vertices[e.mesh.triangles[i]];
        t.b = e.mesh.vertices[e.mesh.triangles[i+1]];
        t.c = e.mesh.vertices[e.mesh.triangles[i+2]];

        color = renderTriangle(st, t, e.m, modelMatrix);
        
        // THE FUCKING Z BUFFER !!!!
        if(color.a > 0.){
            vec3 fragmentPosition = t.ndc.x * t.bV + t.ndc.y * t.cV + t.ndc.z * t.aV;
            float test = length(vec3(0.) - fragmentPosition); 
            if (test < pixel.depth) { 
                pixel.depth = test; 
                pixel.color = color; 
            }
        }
    }
}

void renderWorld(vec2 st, World w){
    for(int i = 0; i < MAX_ENTITY_NUMBER; i++){
        if(i >= w.entitiesLen) { break; }

        renderEntity(st, w.entities[i]);
    }
}

/* ====================== MAIN ====================== */

void defineCube(inout Mesh m) {

    // vertices
    m.vertices[0] = vec3(-0.5, 0.5, -0.5); // topFL
    m.vertices[1] = vec3(0.5, 0.5, -0.5); // topFR
    m.vertices[2] = vec3(0.5, 0.5, 0.5); // topBR
    m.vertices[3] = vec3(-0.5, 0.5, 0.5); // topBL

    m.vertices[4] = vec3(-0.5, -0.5, -0.5); // BotFL
    m.vertices[5] = vec3(0.5, -0.5, -0.5); // BotFR
    m.vertices[6] = vec3(0.5, -0.5, 0.5); // BotBR
    m.vertices[7] = vec3(-0.5, -0.5, 0.5); // BotBL
    m.verticesLength = 8;
    
    int tri[36] = int[36](
        // Top
        2,1,0,
        3,2,0,
        // Bottom
        4,5,6,
        4,6,7,
        // Front
        0,1,4,
        1,5,4,
        // Back
        7,6,2,
        7,2,3,
        // Right
        1,2,5,
        5,2,6,
        // Left
        7,3,0,
        0,4,7
    );

    for (int i = 0; i < 36; ++i) m.triangles[i] = tri[i];

    m.trianglesLength = 36;
}

void defineGr(inout Mesh m) {
    // Vertices
    m.vertices[0] = vec3(-1, 0, 1); // FL
    m.vertices[1] = vec3(1, 0, 1); // FR
    m.vertices[2] = vec3(1, 0, -1); // BR
    m.vertices[3] = vec3(-1,0 , -1); // BL

    m.verticesLength = 4;
    
    // Triangles index 
    int tri[6] = int[6](
        0,1,2,
        0,2,3
    );

    for (int i = 0; i < 6; ++i) m.triangles[i] = tri[i];

    m.trianglesLength = 6;
}

void main() {

    vec2 st = gl_FragCoord.xy/u_resolution.xy;
    vec2 normalizedMouse = normalize(u_mouse);

    pixel.color = vec4(0.);
    pixel.depth = 1e9;

    // Define cam
    world.camera = Camera(vec3(0.,2., -3.),
                vec3(0.0, 0.0, 0.0),
                vec3(0.0, 1.0, 0.0),
                mat4(0.), mat4(0.));     
    
    world.camera.viewMatrix = getViewMatrix(world.camera.position, world.camera.target, world.camera.up);
    world.camera.projectionMatrix = getPerspectiveMatrix(0.5*PI, u_resolution.x / u_resolution.y, 0.1, 100.0);

    world.skyboxColor = vec4(0.1, 0.4, 0.5, 1.0);

    // Define light source
    Light light;
    light.position = vec3(-2, 0., -0.);
    light.color = vec3(1.0, 1.0, 1.0);
    light.intensity = 7.;

    AddLight(world, light);

    Light lightB;
    lightB.position = vec3(2, 0., -0.);
    lightB.color = vec3(1.0, 1.0, 1.0);
    lightB.intensity = 7.;

    AddLight(world, lightB);

    // Define Cube
    Entity cube; 
    defineCube(cube.mesh); // Init geometry 

    cube.m.color = vec4(0.4, 0.0, 0.6, 1.);
    cube.m.reflection = 0.1;
    cube.m.transparency = 0.;

    cube.transform = default_transform;
    rotateY(cube.transform, 0.5 * u_time);
    rotateZ(cube.transform, 0.5);
    cube.transform.pos = vec3(.0);
    //scaleUniform(cube.transform, 0.8);
    
    AddEntity(world, cube);

    // Define Ground
    Entity gr; 
    defineGr(gr.mesh); // Init geometry 
    gr.m.color = vec4(0.8, 0.8, 0.8, 1); 
    gr.m.reflection = 0.8;
    gr.transform = default_transform;
    gr.transform.pos = vec3(0., 0., 0.);
    rotateY(gr.transform, 0.5 * - u_time);
    //scale(gr.transform, vec3(2, 1., 2));
    AddEntity(world, gr);


    // Render
    renderWorld(st, world);
    
    vec4 color = pixel.color;
    // Result
    fragColor = color.a > 0.0 ? color : world.skyboxColor;
}
