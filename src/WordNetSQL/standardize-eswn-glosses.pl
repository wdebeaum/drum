#!/usr/bin/perl -p

chomp;

# weird quote characters, not sure what character set they come from.
# In the original, they're just bytes 0x93 and 0x94, but iconv adds a 0xc2
# prefix to make them UTF-8-ish.
s/\xc2(\x93|\x94)/"/g;

# for some reason #s denote examples, and sometimes have extra 0#s
s/#(0#)+/#/g;
s/#$//; # remove final #
s/#/; "/; # convert the first # to the boundary between defs and examples
s/#/"; "/g; # convert the rest to boundaries between examples

s/: "/; "/; # a different def-ex boundary

$_ .= '"' if (@{[ /"/g ]} % 2 == 1); # add a final " if we have an odd number

# put quotes around ;s within examples
1 while (s/(".*?[^"]); /$1"; "/);

$_ .= "\n";

__END__

Test Cases

Realidad con o sin vida#0#0#
Realidad con o sin vida

Cualquier entidad viva
Cualquier entidad viva

Seres vivos considerados colectivamente#no hay vida en Marte#vida extraterrestre#
Seres vivos considerados colectivamente; "no hay vida en Marte"; "vida extraterrestre"

Unidad estructural y funcional básica de todos los organismos; las células pueden existir como unidades de vida independientes o pueden formar colonias o tejidos como en las plantas y los animales superiores
Unidad estructural y funcional básica de todos los organismos; las células pueden existir como unidades de vida independientes o pueden formar colonias o tejidos como en las plantas y los animales superiores

Que se dirige a un final: "sus últimos años"
Que se dirige a un final; "sus últimos años"

Individuo de la especie humana#había demasiado trabajo para una sola persona#persona física; persona jurídica#
Individuo de la especie humana; "había demasiado trabajo para una sola persona"; "persona física"; "persona jurídica"

Completo en extensión o grado: <U+0093>un desastre total<U+0094>; <U+0093>la verdad absoluta<U+0094>
Completo en extensión o grado; "un desastre total"; "la verdad absoluta" 

