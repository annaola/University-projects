'''''''''''''''''

Kierunki:

1 = +x, -1 = -x
2 = +y, -2 = -y
3 = +z, -3 = -z

zolw = [polozenie, glowa, brzuch, prawy_bok]
poczatkowo: zolw = [[0,0,0], -1, -2, -3]

'''''''''''''''''

def prawo(zolw):
    pom = zolw[1] #zapamietuje pozycje glowy
    zolw[1] = zolw[3] #glowa zostaje obrocona na miejsce prawego boku
    zolw[3] = -pom #prawy bok zostaje obrocony w przeciwna strone niz byla glowa
    return zolw

def lewo(zolw): #analogicznie do "prawo"
    pom = zolw[3]
    zolw[3] = zolw[1]
    zolw[1] = -pom
    return zolw

def gora(zolw):
    pom = zolw[2] #zapamietuje pozycje brzucha
    zolw[2] = zolw[1] #brzuch zostaje obrocony na miejsce glowy
    zolw[1] = -pom #glowa zostaje obrocona w przeciwna strone niz byl brzuch
    return zolw

def dol(zolw): #analogicznie do "gora"
    pom = zolw[1]
    zolw[1] = zolw[2]
    zolw[2] = -pom
    return zolw

def obrot_p(zolw):
    pom = zolw[3] #zapamietuje pozycje prawego boku
    zolw[3] = zolw[2] #prawy bok zostaje obrocony na miejsce brzucha
    zolw[2] = -pom #brzuch zostaje obrocony w przeciwna strone niz byl prawy bok
    return zolw

def obrot_l(zolw): #analogicznie do "obrot_p"
    pom = zolw[2]
    zolw[2] = zolw[3]
    zolw[3] = -pom
    return zolw

def ruch(zolw, d):
    if zolw[1] < 0:
        zolw[0][-zolw[1]-1] -= d #zmniejsza odpowiednia wspolrzedna
    else:
        zolw[0][zolw[1]-1] += d
    return zolw

