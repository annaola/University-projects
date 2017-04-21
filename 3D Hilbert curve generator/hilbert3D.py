from ruchy_zolwia import *
from math import *

def d_odcinka(krawedz, l_rzedow): #dlugosc odcinka laczacego dwa punkty
    return (krawedz * 1.0) / ((2 ** l_rzedow - 1) * 1.0)

def wspolrzedne_pocz(krawedz): #wspolrzedne poczatkowego polozenia zolwia (srodek szescianu w punkcie (0, 0, 0)
    x = krawedz / 2.0
    return [x, x, x]

def obrot_OX(punkt, kat): #obrot szescianu wokol wspolrzednej OX
    r = radians(kat) #zamiana kata ze stopni na radiany
    y = punkt[1] * cos(r) + punkt[2] * sin(r)
    z = punkt[2] * cos(r) - punkt[1] * sin(r)
    return [punkt[0],y,z]

def obrot_OY(punkt, kat): #obrot szescianu wokol wspolrzednej OY
    r = radians(kat)
    x = punkt[0] * cos(r) + punkt[2] * sin(r)
    z = punkt[2] * cos(r) - punkt[0] * sin(r)
    return [x,punkt[1],z]

def przesuniecie(punkt, x, y, z): #przesuniecie szescianu tak, aby jego srodek znajdowal sie w punkcie (x, y, z)
    a = punkt[0] + x
    b = punkt[1] + y
    c = punkt[2] + z
    return [a,b,c]

def rzut(punkt, d): #rzut punktu w przestrzeni na plaszczyzne przechodzaca przez punkt (0,0,0), gdy obserwator jest ustawiony w odleglosci d od plaszczyzny
    s = punkt[2]/((punkt[2] + d)*1.0)
    a = punkt[0] * (1 - s)
    b = punkt[1] * (1 - s)
    return [a, b]

def modyfikacja(punkt, psi, fi, x, y, z, d): #zlozenie obrotow i przesuniecia oraz zrzutowanie punktu na plaszczyzne
    e = obrot_OY(punkt, psi)
    e = obrot_OX(e, fi)
    e = przesuniecie(e, x, y, z)
    e = rzut(e, d)
    return e

def krzywa(odcinek, n, dane, d, x, y, z, fi, psi):
    if n == 0:
        pass
    else:
        gora(dane)
        obrot_p(dane)
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        gora(dane)
        obrot_p(dane)
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        lewo(dane)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        obrot_p(dane)
        obrot_p(dane)
        dol(dane)
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        dol(dane)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        obrot_p(dane)
        obrot_p(dane)
        lewo(dane)
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        lewo(dane)
        obrot_l(dane)
        ruch(dane, odcinek)
        a = modyfikacja(dane[0], psi, fi, x, y, z, d)
        print "%f %f lineto" % tuple(a)
        
        krzywa(odcinek, n-1, dane, d, x, y, z, fi, psi)
        lewo(dane)
        obrot_l(dane)

def naglowek(s):
    x = s/2
    print "%!PS-Adobe-2.0 EPSF-2.0\n%%BoundingBox:", "-%d -%d %d %d" % (x, x, x, x)

def stopka():
    print ".6 setlinewidth\nstroke\nshowpage\n%%Trailer\n%EOF\n"

def hilbert3D(n, s, u, d, x, y, z, fi, psi):
    zolw = [wspolrzedne_pocz(u), -3, 2, -1] #poczatkowe ustawienie "zolwia"
    odcinek = d_odcinka(u, n) #dlugosc odcinka w krzywej
    e = modyfikacja(zolw[0], psi, fi, x, y, z, d) #polozenie punktu po przeksztalceniach
    naglowek(s)
    print "%f %f moveto" % tuple(e) #wypisanie na wyjsciu punktu
    krzywa(odcinek, n, zolw, d, x, y, z, fi, psi)
    stopka()
    
hilbert3D(3, 1000, 300, 400, 0, 0, 0, 32.5, -38)
