import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
    providedIn:'root'
})
export class PathService{

    constructor(){}

    async getPath(){
        const url = 'http://localhost:3000/api/path/all'
        let test : any[]=[];
        await fetch(url,{
            method:'GET',
            headers:{
                'Accept': 'application/json'
            }
        }).then(res => res.json().then(data=>{test=data;}));

        return test;
    }

    async createPath(path:any){
        const url= 'http://localhost:3000/api/path/'

        const data = path;

        fetch(url,{
            method: 'POST',
            body: JSON.stringify(data),
            headers:{
                'Content-Type': 'application/json'
            },
        })
    }
}