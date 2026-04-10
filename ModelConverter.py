import trimesh
import numpy as np
from PIL import Image
import sys

def convert_to_texture(file_path, output_name="mesh.png"):
    try:
        mesh = trimesh.load(file_path)
    except Exception as e:
        print(f"Error loading file: {e}")
        return

    if isinstance(mesh, trimesh.Scene):
        mesh = mesh.dump(concatenate=True)

    mesh.process(validate=True)

    mesh.apply_translation(-mesh.centroid)
    
    max_dist = np.max(np.abs(mesh.vertices))
    if max_dist > 0:
        scale_factor = 1.8 / max_dist
        mesh.apply_scale(scale_factor)

    vertices = mesh.vertices
    faces = mesh.faces

    if len(vertices) > 256:
        print(f"CRITICAL ERROR: Model has {len(vertices)} vertices.")
        return

    print(f"Processing: {len(vertices)} vertices, {len(faces)} triangles.")

    width = max(len(vertices), len(faces))
    img = Image.new('RGB', (width, 2), color='black')
    pixels = img.load()

    for x in range(len(vertices)):
        v = vertices[x]
        r = int(((v[0] + 2.0) / 4.0) * 255)
        g = int(((v[1] + 2.0) / 4.0) * 255)
        b = int(((v[2] + 2.0) / 4.0) * 255)
        pixels[x, 0] = (np.clip(r, 0, 255), 
                        np.clip(g, 0, 255), 
                        np.clip(b, 0, 255))

    for x in range(len(faces)):
        t = faces[x]
        pixels[x, 1] = (int(t[0]), int(t[1]), int(t[2]))

    img.save(output_name)
    print(f"Success! {output_name} generated.")
    print(f"trianglesLength = {len(faces)};")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py model.obj [output.png]")
    else:
        out = sys.argv[2] if len(sys.argv) > 2 else "mesh.png"
        convert_to_texture(sys.argv[1], out)