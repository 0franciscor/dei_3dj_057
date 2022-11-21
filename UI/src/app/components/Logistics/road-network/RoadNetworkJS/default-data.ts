import * as THREE from "three";

export const nodeData = {
    scale: new THREE.Vector3(1.0, 1.0, 1.0)
}


export const warehousePosition = {

    /*
    1 Arouca -50.0000 -42.6618 15.6250
    2 Espinho 26.6951 -36.7615 34.3750
    3 Gondomar 50.0000 50.0000 12.5000
    4 Maia 22.8206 -19.4217 43.7500
    5 Matosinhos 37.4080 -22.8394 21.8750
    6 Oliveira de Azeméis -5.0756 -50.0000 46.8750
    7 Paredes -33.4754 -21.2052 0.0000
    8 Porto 24.3898 -24.9214 37.5000
    9 Póvoa de Varzim 49.9225 -7.4403 25.0000
    10 Santa Maria da Feira 8.7369 -43.0783 6.2500
    11 Santo Tirso -5.6955 -10.3708 40.6250
    12 São João da Madeira -2.4215 -45.1446 18.7500
    13 Trofa 11.0035 -10.6851 28.1250
    14 Vale de Cambra -20.8446 -49.6622 3.1250
    15 Valongo -0.9492 -22.5016 50.0000
    16 Vila do Conde 47.4041 -9.6952 9.3750
    17 Vila Nova de Gaia 21.0384 -27.5927 31.2500
    */

    matrix: [
        [-50.0,-42.6618,15.6250],
        [26.6951,-36.7615,34.3750],
        [50.0,50.0,12.5],
        [22.8206,-19.4217,43.75],
        [37.4080,-22.8394,21.875],
        [-5.0756,-50.0,46.875],
        [-33.4754,-21.2052,0.0],
        [24.3898,-24.9214,37.5],
        [49.9225,-7.4403,25.0],
        [8.7369,-43.0783,6.25],
        [-5.6955,-10.3708,40.625],
        [-2.4215,-45.1446,18.75],
        [11.0035,-10.6851,28.125],
        [-20.8446,-49.6622,3.125],
        [-0.9492,-22.5016,50.0],
        [47.4041,-9.6952,9.375],
        [21.0384,-27.5927,31.25]
    ]
        

}



export const warehouseConnections = {

    /*
    1 to 4
    1 to 6
    2 to 10
    2 to 13
    3 to 1
    3 to 6
    4 to 13
    4 to 12
    5 to 10
    5 to 15
    6 to 16
    6 to 5
    7 to 17
    7 to 8
    8 to 9
    8 to 1
    9 to 2
    9 to 15
    10 to 2
    10 to 12
    11 to 16
    11 to 6
    12 to 8
    12 to 15
    13 to 3
    13 to 15
    14 to 9
    14 to 8
    15 to 3
    15 to 12
    16 to 4
    16 to 17
    17 to 1
    17 to 2
    */
    
    
    
    matrix: [
        [1,4,getRandomNumber()],
        [1,6,getRandomNumber()],
        [2,10,getRandomNumber()],
        [2,13,getRandomNumber()],
        [3,1,getRandomNumber()],
        [3,6,getRandomNumber()],
        [4,13,getRandomNumber()],
        [4,12,getRandomNumber()],
        [5,10,getRandomNumber()],
        [5,15,getRandomNumber()],
        [6,16,getRandomNumber()],
        [6,5,getRandomNumber()],
        [7,17,getRandomNumber()],
        [7,8,getRandomNumber()],
        [8,9,getRandomNumber()],
        [8,1,getRandomNumber()],
        [9,2,getRandomNumber()],
        [9,15,getRandomNumber()],
        [10,2,getRandomNumber()],
        [10,12,getRandomNumber()],
        [11,16,getRandomNumber()],
        [11,6,getRandomNumber()],
        [12,8,getRandomNumber()],
        [12,15,getRandomNumber()],
        [13,3,getRandomNumber()],
        [13,15,getRandomNumber()],
        [14,9,getRandomNumber()],
        [14,8,getRandomNumber()],
        [15,3,getRandomNumber()],
        [15,12,getRandomNumber()],
        [16,4,getRandomNumber()],
        [16,17,getRandomNumber()],
        [17,1,getRandomNumber()],
        [17,2,getRandomNumber()]
    ]


    
        

}

function getRandomNumber() {
    //return random number between 0.1 and 0.8
    return Math.random() * (0.8 - 0.1) + 0.1;
}