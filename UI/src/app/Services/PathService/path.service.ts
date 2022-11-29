import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
    providedIn:'root'
})
export class PathService{

    public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
    constructor(){}

    async getPath(warehouses:any){
        if(warehouses.startWHId == ''){
            warehouses.startWHId = "undefined"
        }else if(warehouses.destinationWHId ==''){
            warehouses.destinationWHId = "undefined"
        };

        const url = 'http://localhost:3000/api/path/all/'+warehouses.startWHId +"/"+warehouses.destinationWHId;
        let test : any[]=[];
        const data = warehouses
        
       
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