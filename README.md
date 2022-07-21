# CLAUSTRO

## Foro

Al recibir las instrucciones para la prueba técnica, vi que el objetivo era desarrollar
una simplificación de un foro (cogiendo los posts de la api de `jsonplaceholder`).

Después de meditarlo un rato, decidí que iba a desarrollar un foro hecho y derecho, con backend y todo. 
La decisión surge de que hace no mucho comencé a explorar **Next.js**, pero la web que desarrollé en ese
momento era muy sencilla ([se puede ver aquí](https://ioracle.es)) y no tiene api ni rutas (es una sola página).

Precisamente por querer desarrollar un foro enteramente funcional, necesitaba un backend que funcionara,
con lo que me resultaba muy conveniente usar Nextjs. Además, he aprendido mucho más de nextjs 
que lo que aprendí al hacer la otra página, que es demasiado sencilla (aunque me llevó a tener un primer
contact con nextjs porque también necesitaba un backend para recibir el formulario de contacto).


## Foro interno

Habiendo ya decidido que iba a desarrollar un foro, decidí que quería que fuese "interno", es decir, 
que sea como un foro interno de la empresa, donde se registre a los trabajadores y no se pueda registrar
cualquiera. Iba a llamarse "Ágora", pero lo cambié a "Claustro" cuando decidí que sería interno.

En la práctica, esto se traduce a que un usuario que no está logueado solo puede ver el formulario de "sign-in"
y no puede ver nada más, ni siquiera una página para registrarse en el foro. La página para registrar nuevos usuarios solamente 
es accesible para usuarios que ya se han logueado.

Por tanto:

# IMPORTANTE

Existen en este momento varios usuarios ya creados en la base de datos (modo development):

1. email: `aaa@aaa`, contraseña: `aaa`
2. email: `bbb@bbb`, contraseña: `bbb`
3. email: `ccc@ccc`, contraseña: `ccc`
4. email: `ddd@ddd`, contraseña: `ddd`
5. email: `eee@eee`, contraseña: `eee`
6. email: `fff@fff`, contraseña: `fff`

Y para ver cualquier página distinta al login hay que iniciar sesión con alguna de esas cuentas.
Además, para poder conectarse a la base de datos, hay que añadir un archivo `.env.local` a la raíz
del proyecto, que contiene la url de la base de datos.

## Dos comentarios más

### Sobre typescript:

Entiendo que las instrucciones exigían un uso de Typescript mayor del que yo he tenido, pero
prefería concentrarme en Next.js y terminar el proyecto a tiempo, y por eso he tenido poco en cuenta el uso
de Typescript. Tampoco hay tests (pero esos eran opcionales).

### Sobre el paginador de los posts en cada hilo:

Esto es lo útimo que he hecho antes de entregar el proyecto, añadir un límite de 10 posts mostrados 
e incluir un navegador (paginador) que permita cambiar de "página" y mostrar los 10 siguientes posts.

Primero pensé en desarrollar yo mismo un componente desde 0 para paginar, porque una vez que lo tenga hecho ya 
lo tengo para futuros proyectos (eso me recuerda que debería comentar que una parte importante del código de este
proyecto la he cogido de distintos proyectos anteriores, es humanamente imposible generar todo este código en 10 días).

Pero luego, como estaba un poco hasta las narices ya del foro, y sabiendo que el approach más "profesional" es usar una librería
para ahorrarse todo el trabajo posible, decidí buscar en npm.

La primera que quise probar es `react-js-pagination`, pero al instalarla se me avisó de que tiene "2 high severity vulnerabilities",
y como no me gusta que el ordenador me informe de que existen vulnerabilidades graves, decidí usar otra sin probar esa.

Después probé con `react-paginate`, y la instalación fue bien, pero he calcado el ejemplo que hay en la documentación, 
y funciona muy mal (muchos bugs). Consecuentemente, en este momento pienso en que debería volver a la idea inicial de 
implementar un paginador yo mismo, aunque ya me he dado cuenta de que es considerablemente más complejo de lo que puede parecer a simple vista (como suele pasar).

En cualquier caso, voy a entregar el proyecto tal como está ahora mismo, porque no deja de ser una prueba técnica que no va a usar nadie
en la vida real. Es posible que en los próximos días escriba el paginador por aburrimiento.