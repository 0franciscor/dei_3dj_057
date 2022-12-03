import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
    providedIn:'root'
})
export class PathService{

    public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
    constructor(){}

    async getAllPaths(warehouses:any){
        if(warehouses.startWHId == ''){
            warehouses.startWHId = "undefined"
        }else if(warehouses.destinationWHId ==''){
            warehouses.destinationWHId = "undefined"
        };
        let paths = []
        const url = this.urlOrigin+'api/path/all/'+warehouses.startWHId +"/"+warehouses.destinationWHId;
        let test : any[]=[];
        const data = warehouses
        const response= await this.sendFetch(url,'GET',null)
        console.log(response)
        if(response.status != 200){
         console.log("No paths found")
        }
        paths= await response.json();
        return paths;
    }

    async createPath(path:any){
        const url= this.urlOrigin+'api/path/'
        const data = path;
        

      const response = await this.sendFetch(url,'POST',data);
      return response;
    }

    async createPathProlog(path:any){
      const url= this.urlOrigin+'api/path/prolog'
      const data = path;
      
      const response = await this.sendFetch(url,'POST',data);
      return response;
    }

    async sendFetch(url: string, method: string, data: any) {
        if(data)
          return fetch(url, {
            method: method,
            body: JSON.stringify(data),
            headers: {
              'Content-Type': 'application/json'
            },
          })
        else
          return fetch(url, {
            method: method,
            headers: {
              'Accept': 'application/json'
            }
          })
    }
}