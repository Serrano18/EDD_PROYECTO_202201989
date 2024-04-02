# Manual Técnico - Proyecto de Gestión de Imágenes
### 202201989 - María Patricia Serrano Ramírez
## Descripción del Proyecto

El proyecto de Gestión de Imágenes es un sistema desarrollado en Fortran que permite la gestión y manipulación de imágenes digitales, organizadas en álbumes, para usuarios registrados. El sistema utiliza varias estructuras de datos para almacenar y manipular la información de manera eficiente.

## Estructuras de Datos Utilizadas

### Árbol B

El Árbol B se utiliza para guardar los clientes registrados en el sistema. Cada nodo del árbol contiene información sobre un cliente, como su nombre, número de teléfono y dirección.

### Matriz Dispersa

La Matriz Dispersa se utiliza para guardar la información de una capa de imágenes. Cada elemento de la matriz representa un píxel de la imagen, con información sobre su color y posición en la imagen.

### Árbol Binario de Búsqueda (ABB)

El Árbol Binario de Búsqueda se utiliza para guardar las capas con las cuales se pueden generar distintas imágenes. Cada nodo del árbol contiene información sobre una capa, como su nombre y tamaño.

### Árbol AVL

El Árbol AVL se utiliza para guardar cada una de las imágenes de un cliente. Cada nodo del árbol contiene información sobre una imagen, como su nombre, tamaño y formato.

### Lista Circular Doblemente Enlazada

La Lista Circular Doblemente Enlazada se utiliza para guardar el listado de álbumes por cada cliente. Cada nodo de la lista representa un álbum y contiene una sublista de imágenes pertenecientes a ese álbum.

## Funcionalidades Principales

- Registro de nuevos clientes.
- Creación de álbumes y subida de imágenes.
- Generación de imágenes a partir de capas.
- Visualización y descarga de imágenes.

## Descripción de Módulos

### Módulo Árbol Binario de Búsqueda (arbolbb)

El módulo `arbolbb` incluye tipos de datos y métodos para manejar un árbol binario de búsqueda de capas. Algunos métodos importantes son:

- `insertar`: Inserta una nueva capa en el árbol.
- `eliminar`: Elimina una capa del árbol.
- `buscar`: Busca una capa en el árbol.
- `recorrer`: Recorre el árbol en orden.

### Módulo Matriz Dispersa (matriz_dispersa)

El módulo `matriz_dispersa` incluye tipos de datos y métodos para manejar una matriz dispersa de píxeles de imagen. Algunos métodos importantes son:

- `insertar_pixel`: Inserta un nuevo píxel en la matriz.
- `eliminar_pixel`: Elimina un píxel de la matriz.
- `obtener_pixel`: Obtiene la información de un píxel en la matriz.

## Requerimientos del Sistema

- Sistema Operativo: Windows, Linux o macOS.
- Compilador Fortran.
- Bibliotecas estándar de Fortran.

## Instalación

1. Descargar el código fuente del proyecto.
2. Compilar los módulos necesarios utilizando el compilador Fortran.
3. Ejecutar el programa principal para iniciar el sistema.

## Conclusiones

El proyecto de Gestión de Imágenes es un sistema completo y eficiente para la gestión de imágenes digitales. Utiliza diversas estructuras de datos y algoritmos para garantizar un rendimiento óptimo en el manejo de la información.
