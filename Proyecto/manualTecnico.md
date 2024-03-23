### Manual Técnico

#### Descripción General
El programa simula el funcionamiento de un sistema de atención de clientes que desean imprimir imágenes. Utiliza estructuras de datos como colas, listas enlazadas y pilas para gestionar la llegada de clientes, su atención en ventanillas, la impresión de imágenes y el registro de clientes atendidos.

#### Estructuras Utilizadas
1. **Cola de Recepción:** Almacena clientes que llegan a la empresa.
2. **Lista de Ventanillas:** Lista simplemente enlazada que representa las ventanillas disponibles.
3. **Pilas de Imágenes:** Cada ventanilla tiene una pila para recibir las imágenes a imprimir.
4. **Lista de Clientes Atendidos:** Lista simplemente enlazada que guarda la información de los clientes atendidos.
5. **Cola de Impresión:** Cada impresora tiene una cola de impresión para gestionar las imágenes a imprimir.
6. **Lista de Clientes en Espera:** Lista doblemente enlazada que almacena clientes que esperan ser atendidos.

#### Funcionalidades
- **Carga Masiva de Clientes:** Permite cargar clientes desde un archivo JSON.
- **Cantidad de Ventanillas:** Permite especificar la cantidad de ventanillas disponibles.
- **Ejecutar Paso:** Simula el paso de tiempo y la atención de clientes en ventanillas.
- **Estado en Memoria de las Estructuras:** Muestra el estado actual de las estructuras de datos en memoria.
- **Reportes:** Genera reportes de clientes con mayor cantidad de imágenes impresas y otras estadísticas.
- **Datos Estudiantiles:** Muestra información del desarrollador del programa.
- **Salir:** Finaliza la ejecución del programa.

#### Implementación
El programa está implementado en Fortran y utiliza módulos para organizar las estructuras de datos y subrutinas para las diferentes acciones que se realizan en el sistema de atención de clientes.

#### Uso
- Al iniciar el programa, se muestra un menú principal con las opciones disponibles.
- Se pueden cargar clientes, especificar la cantidad de ventanillas, ejecutar pasos de tiempo, ver el estado de las estructuras, generar reportes y obtener información del desarrollador.
- Se utilizan diferentes estructuras de datos y algoritmos para simular el proceso de atención de clientes en una empresa de impresión.

#### Nota
- Es importante compilar el programa en un entorno de desarrollo Fortran adecuado para su correcta ejecución.
- Se recomienda seguir las indicaciones del programa para cargar clientes, ejecutar pasos y generar reportes de manera adecuada.
