# Rasterizer GLSL 

## Description 

Ce projet est l'implémentation d'un **rasterizer logiciel basique** dans un **fragment shader GLSL** . 

Il gere le rendu des triangles 3D avec projection perspective, éclairage et back-face culling, et supporte plusieurs entités et sources lumineuses.

C’est une mini pipeline graphique GPU reproduit manuellement dans un fragment shader. 

## Comment testé 

Le moyen le plus simple est d'installé l'extention vscode "Glsl canvas", ouvrir le shader, ctrl+Shift+P => Show GLSLCanvas

## Fonctionnalitées 

- [x] Rendu de triangle 3D : tranformation des somet via matrice de vue et projection.

- [x] Application d'une matrice de transformation 

- [x] Back-Face Culling : ignore automatiquement les triangle orienté a l'opposé de la camera 

- [x] Eclairage : Gestion d'un eclairage diffus simple avec attenuation
  
- [x] Support d'entité multiple 

- [x] Z-Buffer : Gère correctement le recouvrement entre objets.

- [x] Support d'eclairage multiple

- [ ] Shading ? 