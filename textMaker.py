from PIL import Image

vertices = [
    # top face vertices (0-5)
    [-0.7, 1.0, 1.0], [-0.7, 1.0, 0.3], [1.0, 1.0, 1.0], [0.3, 1.0, 0.3],
    [1.0, 1.0, -1.0], [0.3, 1.0, -1.0],
    # front face vertices (6-9)
    [1.0, -1.0, -1.0], [0.3, -0.3, -1.0], [-1.0, -0.3, -1.0], [-1.0, -1.0, -1.0],
    # right face vertices (10-15)
    [-1.0, -0.3, 0.3], [-1.0, -1.0, 1.0], 
    [-1.0, 0.3, 0.3], [-1.0, 1.0, 1.0],
    [-1.0, 1.0, -0.1], [-1.0, 0.3, -0.1], 
    # cube vertices (16-21)
    # right top left   right bot right    right bot left 
    [-1.0, 1.0, -1.0], [-1.0, 0.1, -0.1], [-1.0, 0.1, -1.0], 
    # front bot right   front top right   
    [-0.1, 0.1, -1.0], [-0.1, 1.0, -1.0], 
    # top back right
    [-0.1, 1.0, -0.1],

    # offset top vertices (22-27)
    [-0.7, 0.95, 1.0], [-0.7, 0.95, 0.3], [1.0, 0.95, 1.0], [0.3, 0.95, 0.3],
    [1., 0.95, -0.95], [0.3, 0.95, -0.95],
    # offset front vertices (28-31)
    [1.0, -1.0, -0.95], [0.3, -0.3, -0.95], [-0.95, -0.3, -0.95], [-0.95, -1.0, -0.95],
    # offset right vertices (32-37)
    [-0.95, -0.3, 0.3], [-0.95, -1.0, 1.0], 
    [-0.95, 0.3, 0.3], [-0.95, 1.0, 1.0],
    [-0.95, 1.0, -0.1], [-0.95, 0.3, -0.1], 
    # back cube (38)
    [-0.1, 0.1, -0.1]
]

indices = [
    # top
    2,1,0, 2,3,1, 2,4,3, 4,5,3,              
    # front
    4,6,5, 5,6,7, 9,7,6, 9,8,7,
    # right
    9,11,10, 8,9,10, 
    11,13,12, 10,11,12,
    13,14,15, 12,13,15,
    
    14,16,17, 18,17,16, 20,19,16, 19,18,16,
    14,21,20, 14,20,16,
    
    # offset top
    22,23,24, 23,25,24, 25,26,24, 25,27,26,
    # offset front
    27,28,26, 29,28,27, 28,29,31, 29,30,31,  
    # offset right
    32,33,31, 32,31,30,
    34,35,33, 34,33,32,
    37,36,35, 37,35,34,
    
    # back cube left
    38,20,21, 19,20,38,
    # back cube back
    38,21,14, 17,38,14,
    # back cube bot
    18,19,38, 17,18,38,

    # borders
    # top 
    22,0,1, 22,1,23, 23,1,3, 23,3,25,
    25,3,27, 3,5,27, 2,0,22, 2,22,24,
    4,2,26, 2,24,26, 
    # front
    7,29,5, 5,29,27, 7,8,29, 29,8,30,
    4,26,28, 4,28,6, 9,6,28, 9,28,31,
    # right
    30,8,32, 8,10,32, 33,9,31, 33,11,9,
    10,12,34, 10,34,32, 13,33,35, 13,11,33,
    14,13,36, 36,13,35, 12,15,37, 12,37,34, 
]

# Group indices into triangles (3 indices per pixel)
triangles = [indices[i:i+3] for i in range(0, len(indices), 3)]

# Image dimensions: Width must fit the largest array.
width = max(len(vertices), len(triangles)) 
img = Image.new('RGB', (width, 2), color='black')
pixels = img.load()

# ROW 0: Encode Vertices
# Mapping float [-2.0, 2.0] -> int [0, 255]
for x, v in enumerate(vertices):
    r = int(((v[0] + 2.0) / 4.0) * 255)
    g = int(((v[1] + 2.0) / 4.0) * 255)
    b = int(((v[2] + 2.0) / 4.0) * 255)
    pixels[x, 0] = (r, g, b)

# ROW 1: Encode Triangles
for x, t in enumerate(triangles):
    pixels[x, 1] = (t[0], t[1], t[2])

img.save('mesh.png')
print(f"mesh.png saved! ({width}x2 pixels)")
print(f"Total Triangles: {len(triangles)}")