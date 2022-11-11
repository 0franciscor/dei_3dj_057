import { Component, OnInit } from '@angular/core';
import * as THREE from 'three';

@Component({
  selector: 'app-road-network',
  templateUrl: './RoadNetworkJS/road-network.component.html',
  styleUrls: ['./road-network.component.css']
})
export class RoadNetworkComponent implements OnInit {

  


  constructor() {
    const scene = new THREE.Scene();
    const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    const renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight-64);
    document.body.appendChild(renderer.domElement);

    const geometryCircle = new THREE.CircleGeometry( 5, 32 );
    const materialCircle = new THREE.MeshBasicMaterial( { color: 0xffff00 } );
    const circle = new THREE.Mesh( geometryCircle, materialCircle );
    circle.position.set(0,0,0);
    scene.add( circle );

    const animate = function () {
        requestAnimationFrame(animate);
        // circle.rotation.x += 0.01;
        // circle.rotation.y += 0.01;

        renderer.render(scene, camera);
    };
    camera.position.z = 10;
    renderer.render(scene, camera);
    animate();
   }

  ngOnInit(): void {

  }

  

  

}
