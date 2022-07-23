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

