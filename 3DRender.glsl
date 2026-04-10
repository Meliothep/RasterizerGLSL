#version 300 es

precision lowp float;

#define PI 3.14159265359
#define MAX_VERTICES_NUMBER 45
#define MAX_TRIANGLES_NUMBER 120 
#define MAX_ENTITY_NUMBER 2 
#define MAX_LIGHT_NUMBER 3 

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

uniform sampler2D u_texture_0;

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

struct Entity {
    int trianglesLength; // Pass the total triangle count (36 for your cube)
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

/* ====================== PIPELINES & RENDER ====================== */

void GeometryPipeline(inout Triangle t, vec2 st, mat4 VM, mat4 MVP, mat3 normalMat) {
    // 1. Object to world 
    t.aC = MVP * vec4(t.a, 1.0);
    t.bC = MVP * vec4(t.b, 1.0);
    t.cC = MVP * vec4(t.c, 1.0);

    // 2. Convert to [0, 1]
    vec2 aN = t.aC.xy / t.aC.w * 0.5 + 0.5;
    vec2 bN = t.bC.xy / t.bC.w * 0.5 + 0.5;
    vec2 cN = t.cC.xy / t.cC.w * 0.5 + 0.5;

    // If the pixel is outside the triangle's rectangle, kill it immediately.
    vec2 minAABB = min(aN, min(bN, cN));
    vec2 maxAABB = max(aN, max(bN, cN));
    
    if (st.x < minAABB.x || st.x > maxAABB.x || 
        st.y < minAABB.y || st.y > maxAABB.y) {
        t.visibility = 0.0;
        return; 
    }

    // 3. Transform to View Space
    vec4 aV4 = VM * vec4(t.a, 1.0);
    vec4 bV4 = VM * vec4(t.b, 1.0);
    vec4 cV4 = VM * vec4(t.c, 1.0);

    t.aV = aV4.xyz / aV4.w;
    t.bV = bV4.xyz / bV4.w;
    t.cV = cV4.xyz / cV4.w;

    // 4. Calculate Normal
    t.normalV = normalize(normalMat * getTriangleNormal(t.a, t.b, t.c));

    // Backface Culling
    vec3 triCenter = (t.aV + t.bV + t.cV) / 3.0;
    vec3 camToTri = normalize(triCenter);
    // is triangle facing away from the camera 
    if (dot(t.normalV, camToTri) >= 0.0) { 
        t.visibility = 0.0;
        return; 
    }

    // 5. Barycentrics
    t.ndc = barycentricCoords(st, aN, bN, cN);
    t.visibility = step(0.0, min(t.ndc.x, min(t.ndc.y, t.ndc.z)));
}

vec4 ColorPipeline(inout Triangle t, Material mat, mat4 V, vec3 fragmentPosition){
    vec4 fragColor = vec4(0.);

    for(int i = 0; i < MAX_LIGHT_NUMBER; i++){
        if(i >= world.lightsLen) { break; }

        vec3 lightPosView = (V * vec4(world.lights[i].position, 1.0)).xyz;
        vec3 lightDir = normalize(lightPosView - fragmentPosition);

        // light culling
        float diffuseIntensity = max(dot(normalize(t.normalV), lightDir), 0.0);
        
        // Attenuation 
        float distance = length(lightPosView - fragmentPosition);
        float attenuation = 1.0 / (distance * distance);

        vec3 baseLighting = diffuseIntensity * world.lights[i].intensity * attenuation * world.lights[i].color;
        vec3 litColor = baseLighting * mat.color.rgb;

        vec3 contrasted = mix(litColor, litColor * litColor * 2.0, mat.reflection);
        vec3 finalColor = contrasted + (attenuation * (1.0/distance) * world.lights[i].color);

        fragColor += vec4(finalColor, 1.0 - mat.transparency);
    } 
    return fragColor;
}

// 2. Update the Renderer to fetch data from the texture
void renderEntity(vec2 st, Entity e) {
    if(e.trianglesLength <= 0) return;

    // Precompute Matrices
    mat4 M = modelMatrix(e.transform);
    mat4 V = world.camera.viewMatrix;
    mat4 P = world.camera.projectionMatrix;
    
    mat4 VM = V * M;          
    mat4 MVP = P * VM;        
    mat3 normalMat = mat3(transpose(inverse(VM))); 

    for(int i = 0; i < MAX_TRIANGLES_NUMBER; i++){
        if(i >= e.trianglesLength) { break; }
        
        // --- READ FROM TEXTURE ---
        
        // Read 1 pixel from ROW 1 (Y=1). This gives us our 3 indices (R, G, B)
        // texelFetch returns 0.0-1.0, so we multiply by 255 to get the exact integer index.
        ivec3 idx = ivec3(round(texelFetch(u_texture_0, ivec2(i, 0), 0).rgb * 255.0));
        
        // Read 3 pixels from ROW 0 (Y=0) using those indices. 
        // We unpack the color [0.0, 1.0] back to world space [-2.0, 2.0]
        vec3 a = texelFetch(u_texture_0, ivec2(idx.x, 1), 0).rgb * 4.0 - 2.0;
        vec3 b = texelFetch(u_texture_0, ivec2(idx.y, 1), 0).rgb * 4.0 - 2.0;
        vec3 c = texelFetch(u_texture_0, ivec2(idx.z, 1), 0).rgb * 4.0 - 2.0;

        Triangle t;
        t.a = a;
        t.b = b;
        t.c = c;

        GeometryPipeline(t, st, VM, MVP, normalMat);

        if(t.visibility > 0.0){
            vec3 fragmentPosition = t.ndc.x * t.bV + t.ndc.y * t.cV + t.ndc.z * t.aV;
            
            // THE FUCKING Z BUFFER !!!!
            float currentDepth = length(fragmentPosition); 
            if (currentDepth < pixel.depth) { 
                pixel.depth = currentDepth;
                pixel.color = ColorPipeline(t, e.m, V, fragmentPosition); 
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

void main() {

    vec2 st = gl_FragCoord.xy/u_resolution.xy;
    vec2 normalizedMouse = normalize(u_mouse - (u_resolution.xy/2.));

    pixel.color = vec4(0.);
    pixel.depth = 1e9;

    // Define cam
    world.camera = Camera(vec3(0, 0., -4),
                vec3(0.0, 0.0, 0.0),
                vec3(0.0, 1.0, 0.0),
                mat4(0.), mat4(0.));     
    
    world.camera.viewMatrix = getViewMatrix(world.camera.position, world.camera.target, world.camera.up);
    world.camera.projectionMatrix = getPerspectiveMatrix(0.4*PI, u_resolution.x / u_resolution.y, 0.1, 100.0);

    world.skyboxColor = vec4(0.0, 0.0, 0.0, 1.0);

    // Define light source

    Light light;
    light.position = vec3(0.6, 0.9, -3.);
    light.color = vec3(1.0, 1.0, 1.0);
    light.intensity = 7.;
    AddLight(world, light);

    Light lightB;
    lightB.position = vec3(0.6, -3., -1.);
    lightB.color = vec3(1.0, 1.0, 1.0);
    lightB.intensity = 7.;
    AddLight(world, lightB);

    Light lightC;
    lightC.position = vec3(-3., -1.5, -0.5);
    lightC.color = vec3(1.0, 1.0, 1.0);
    lightC.intensity = 7.;
    AddLight(world, lightC);

    // Define Cube Entity
    Entity cube; 
    cube.trianglesLength = 70;
    
    cube.m.color = vec4(0.3, 0.23, 0.67, 1.0);
    cube.m.reflection = .9;
    cube.m.transparency = 0.;
    cube.transform = default_transform;

    rotateY(cube.transform, -0.8);
    rotateX(cube.transform, -0.7);
    cube.transform.pos = vec3(.0);
    scaleUniform(cube.transform, .8);
    rotateY(cube.transform, sin(u_time)/2.);
    rotateX(cube.transform, cos(u_time)/2.);
    
    cube.transform.pos.x += 0.5;

    AddEntity(world, cube);

    // Define Cube2 Entity
    Entity cube2; 
    cube2.trianglesLength = 70;
    
    cube2.m.color = vec4(0.6, 0.2, 0.4, 1.0);
    cube2.m.reflection = .9;
    cube2.m.transparency = 0.;
    cube2.transform = default_transform;

    rotateY(cube2.transform, -0.8);
    rotateX(cube2.transform, -0.7);
    cube2.transform.pos = vec3(.0);
    scaleUniform(cube2.transform, .8);
    rotateY(cube2.transform, -sin(u_time)/2.);
    rotateX(cube2.transform, -cos(u_time)/2.);

    cube2.transform.pos.x += -0.5;

    AddEntity(world, cube2);

    // Render
    renderWorld(st, world);
    
    vec4 color = pixel.color;
    fragColor = color.a > 0.0 ? color : world.skyboxColor;
}
