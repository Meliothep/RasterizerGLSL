import trimesh
import numpy as np
from PIL import Image
import sys

def convert_obj_to_texture(file_path, output_name="mesh.png"):
    try:
        mesh = trimesh.load(file_path)
    except Exception as e:
        print(f"Error loading file: {e}")
        return

    if isinstance(mesh, trimesh.Scene):
        mesh = mesh.dump(concatenate=True)

    # center the model
    mesh.apply_translation(-mesh.centroid)
    
    # scale the model within [-1.8, 1.8]
    max_dist = np.max(np.abs(mesh.vertices))
    if max_dist > 0:
        scale_factor = 1.8 / max_dist
        mesh.apply_scale(scale_factor)

    vertices = mesh.vertices
    faces = mesh.faces # Triangle indices

    # constraint Checks
    if len(vertices) > 256:
        print(f"CRITICAL ERROR: Model has {len(vertices)} vertices.")
        print("The PNG index mapping only supports up to 256 vertices (0-255).")
        print("Please simplify the model in Blender (Decimate modifier) before exporting.")
        return

    print(f"Processing: {len(vertices)} vertices, {len(faces)} triangles.")

    # create the 2-row Texture
    width = max(len(vertices), len(faces))
    img = Image.new('RGB', (width, 2), color='black')
    pixels = img.load()

    # ROW 0: Encode Vertices
    for x in range(len(vertices)):
        v = vertices[x]
        r = int(((v[0] + 2.0) / 4.0) * 255)
        g = int(((v[1] + 2.0) / 4.0) * 255)
        b = int(((v[2] + 2.0) / 4.0) * 255)
        pixels[x, 0] = (np.clip(r, 0, 255), 
                        np.clip(g, 0, 255), 
                        np.clip(b, 0, 255))

    # ROW 1: Encode Triangles 
    for x in range(len(faces)):
        t = faces[x]
        pixels[x, 1] = (int(t[0]), int(t[1]), int(t[2]))

    img.save(output_name)
    print(f"\nSuccess! '{output_name}' generated ({width}x2 pixels).")
    
    # Required updates for 3DRender.glsl 
    print(f"\n--- SHADER UPDATE INSTRUCTIONS ---")
    print(f"\ntrianglesLength = {len(faces) * 3};")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py your_model.obj")
    else:
        convert_obj_to_texture(sys.argv[1], sys.argv[2])