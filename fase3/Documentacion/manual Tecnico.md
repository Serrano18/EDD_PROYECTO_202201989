# Manual Técnico - Proyecto de Gestión de Imágenes
### 202201989 - María Patricia Serrano Ramírez
## Descripción del Proyecto

Pixel Print Studio, una empresa de servicios de impresión con sucursales en todo el país, necesita gestionar el mantenimiento de sus tiendas de manera eficiente. Se propone desarrollar un Sistema de Gestión de Mantenimiento para coordinar y registrar las actividades de mantenimiento y reparación en cada sucursal, incluyendo la asignación de técnicos y los costos asociados. La seguridad y confiabilidad de la información serán prioritarias en el desarrollo del sistema.

### Resumen de Estructuras Utilizadas en el Proyecto

1. **Árbol AVL**
   - Almacena sucursales.
   - Cada nodo contiene información de una sucursal (ID, departamento, dirección, impresoras, contraseña).
   - Permite inserción, eliminación y búsqueda eficiente de sucursales.

2. **Tabla Hash**
   - Almacena técnicos.
   - Clave: DPI del técnico.
   - Valores: Nombres, apellidos, género, dirección, teléfono.
   - Utiliza función de dispersión y resolución de colisiones.

3. **Árbol de Merkle**
   - Mantiene integridad de la información de transacciones.
   - Cada hoja contiene un hash de información de transacción.
   - Nodos internos contienen hashes de nodos hoja para validar información.

4. **Blockchain**
   - Lista enlazada simple de bloques.
   - Bloque contiene índice, timestamp, datos de ruta, nonce, hash previo y hash de Merkle.
   - Utiliza prueba de trabajo para encontrar hash válido.
   - Genera nuevos bloques al encontrar ruta menos costosa.

Estas estructuras se utilizan para gestionar eficientemente sucursales, técnicos y rutas, garantizando integridad y seguridad de la información.


## Descripción de Módulos

### Módulo avlsucursales en Fortran
El tipo sucursal representa una sucursal u oficina. Tiene varias propiedades incluyendo un id, departamento, dirección, contraseña, técnicos e impresoras. La propiedad técnicos es de tipo tecnicos_hash, que se importa del módulo tecnicoshash. El tipo sucursal también contiene varios procedimientos para establecer sus propiedades, inicializarla e imprimir su información.

El tipo nodo representa un nodo en un árbol AVL. Contiene un objeto sucursal, un id1, una altura y punteros a sus hijos izquierdo y derecho. La propiedad altura se usa en árboles AVL para balancear el árbol después de inserciones y eliminaciones.

El tipo sucursalesavl representa un árbol AVL de nodos de sucursal. Contiene un uid, un num (número de nodos) y un puntero a la raíz. También contiene una variedad de procedimientos para manipular el árbol. Estos incluyen procedimientos para recorrer el árbol en preorden, en orden y en postorden; agregar nodos; rotar nodos para balancear el árbol; generar un archivo dot para visualizar el árbol; realizar una búsqueda en amplitud; eliminar nodos; y buscar nodos.

Cada uno de estos procedimientos probablemente contiene la lógica para realizar su operación respectiva en el árbol AVL. Por ejemplo, el procedimiento add probablemente contiene la lógica para insertar un nuevo nodo en el árbol, y el procedimiento buscar probablemente contiene la lógica para buscar un nodo en el árbol.

### Módulo tecnicoshash en Fortran

El tipo tecnicos representa a un técnico. Tiene varias propiedades incluyendo dpi (un entero de 8 bytes), nombre, apellido, género, dirección, teléfono y trabajos cumplidos, que son todos enteros. Las propiedades nombre, apellido, género y dirección son cadenas de caracteres de longitud variable, lo cual se indica por el : en sus declaraciones. El tipo tecnicos también contiene varios procedimientos para establecer sus propiedades (set_dpi, set_nombre, set_apellido, set_genero, set_direc, set_telefono, set_trabajoscumplidos), imprimir su información (imprimir_tecnico), crear un técnico (crear_tecnico), e inicializar (inicializador).

El tipo tecnicos_hash representa una tabla hash de técnicos. Contiene varias propiedades: n y m (que típicamente se usan para representar el tamaño actual y máximo de la tabla hash), mini y maxi (que podrían usarse para representar los valores mínimo y máximo de hash), y h (un arreglo de objetos tecnicos). El tipo tecnicos_hash también contiene varios procedimientos para manipular la tabla hash. Estos incluyen procedimientos para inicializar la tabla hash (init), calcular un valor hash usando división (division), calcular un valor hash usando sondaje lineal (linear), insertar un técnico en la tabla hash (insert), rehashear la tabla hash (rehashing), mostrar la tabla hash (show), buscar un técnico en la tabla hash (buscart), listar técnicos (listadotecnicos), y crear una representación gráfica de la tabla hash (graficoth).

### Módulo rutas en Fortran


#### Tipo ruta
Representa una ruta. Tiene varias propiedades incluyendo sucursal1, sucursal2, distancia, impresoras y peso. Todas las propiedades son enteros y se inicializan a -1. El tipo ruta también contiene varios procedimientos para establecer sus propiedades (set_sucursal1, set_sucursal2, set_distancia, set_impresoras, set_weight), imprimir su información (imprimir_ruta) e inicializarla (inicializarruta).

#### Tipo nodoruta
Representa un nodo en una lista doblemente enlazada. Contiene un objeto ruta (oruta) y punteros al siguiente y anterior nodos en la lista (next y prev).

#### Tipo lista_rutas
Representa una lista doblemente enlazada de nodos nodoruta. Contiene punteros a la cabeza y la cola de la lista (head y tail) y un entero size que representa el número de nodos en la lista. También contiene varios procedimientos para manipular la lista. Estos incluyen procedimientos para agregar un nodo a la lista (addr), remover un nodo de la lista (remove), actualizar el peso de un nodo (actualizar_weight), verificar si la lista está vacía (estavacia) y fusionar dos listas (fusionar).

Cada uno de estos procedimientos probablemente contiene la lógica para realizar su operación respectiva en la lista. Por ejemplo, el procedimiento addr probablemente contiene la lógica para insertar un nuevo nodo en la lista, y el procedimiento remove probablemente contiene la lógica para remover un nodo de la lista.
### Módulo resultados en Fortran


#### Tipo resultado
Representa un resultado. Tiene varias propiedades incluyendo idr, weightr, distanciar, impresiones y padre. Todas las propiedades son enteros. El tipo resultado también contiene un procedimiento para imprimir su información (imprimir).

#### Tipo nodores
Representa un nodo en una lista enlazada. Contiene un objeto resultado (res) y un puntero al siguiente nodo en la lista (next).

#### Tipo listares
Representa una lista enlazada de nodos nodores. Contiene punteros a la cabeza y la cola de la lista (head y tail), un entero size que representa el número de nodos en la lista, y un entero total_weightr que representa el peso total de los resultados en la lista. También contiene procedimientos para insertar un resultado en la lista (insertarResultado) e imprimir la lista (imprimirResultado).

El subrutina imprimir imprime las propiedades de un objeto resultado. La subrutina imprimirResultado imprime el peso total de los resultados en la lista y luego imprime cada resultado en la lista. La subrutina insertarResultado inserta un nuevo resultado en la lista. Crea un nuevo nodo nodores, establece las propiedades del objeto resultado en el nodo basado en las propiedades de un objeto ruta, y luego inserta el nodo en la lista. Si la lista está vacía, el nuevo nodo se convierte en la cabeza y la cola de la lista. De lo contrario, el nuevo nodo se agrega al final de la lista, y se actualiza el peso total de los resultados en la lista.
El módulo `graficar` contiene dos tipos de datos: `nodog` y `grafica`. `nodog` representa un nodo en un grafo, con un identificador entero, una lista de rutas `rutas`, y un puntero al siguiente nodo `next`. `grafica` representa un grafo, con un tamaño entero, un puntero al nodo cabeza `head`, y varios procedimientos para manipular el grafo.

### Módulo graficar en Fortran
Los procedimientos en `grafica` incluyen:

- `insertarg`: Inserta un nodo en el grafo.
- `agregarnodo`: Agrega un nodo al grafo.
- `agregarruta`: Agrega una ruta al grafo.
- `obtenernodo`: Obtiene un nodo del grafo.
- `show`: Imprime el grafo.
- `rutamascorta`: Encuentra la ruta más corta en el grafo.

La función `rutamascorta` parece implementar una variación del algoritmo de Dijkstra para encontrar el camino más corto entre dos nodos en un grafo. Utiliza una cola de prioridad para realizar un seguimiento de los nodos a visitar y actualiza los pesos de las rutas a medida que recorre el grafo.

### Módulo sha256 en Fortran

- `sha256(str)`: Esta función toma una cadena como entrada y devuelve su hash SHA-256.
- `dirty_sha256(str)`: Esta función también calcula el hash SHA-256 de una cadena, pero no realiza una operación de intercambio de bytes.
- `sha256b(str, swap)`: Esta es la función principal que calcula el hash SHA-256. Toma una cadena y un indicador de intercambio como entrada. Si el indicador de intercambio es 1, realiza una operación de intercambio de bytes.
- `swap32(inp)`, `swap64(inp)`, `swap64a(inp)`: Estas funciones realizan operaciones de intercambio de bytes en enteros de 32 bits y 64 bits.
- `ch(a, b, c)`, `maj(a, b, c)`: Estas funciones implementan las operaciones 'Ch' y 'Maj' utilizadas en el algoritmo SHA-256.
- `cs0(a)`, `cs1(a)`, `ms0(a)`, `ms1(a)`: Estas funciones implementan las operaciones 'Sigma0', 'Sigma1', 'sigma0' y 'sigma1' utilizadas en el algoritmo SHA-256.

El algoritmo SHA-256 funciona inicializando un conjunto de ocho palabras de 32 bits con ciertos valores fijos, luego procesando los datos de entrada en fragmentos de 512 bits. Para cada fragmento, expande el fragmento en una secuencia de 64 palabras de 32 bits, luego actualiza las ocho palabras usando una serie de operaciones a nivel de bits. El hash final es la concatenación de los valores finales de las ocho palabras.

## Requerimientos del Sistema

- Sistema Operativo: Windows, Linux o macOS.
- Compilador Fortran.
- Bibliotecas estándar de Fortran.

## Instalación

1. Descargar el código fuente del proyecto.
2. Compilar los módulos necesarios utilizando el compilador Fortran.
3. Ejecutar el programa principal para iniciar el sistema.
