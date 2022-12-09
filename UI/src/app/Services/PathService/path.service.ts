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
        let url = this.urlOrigin+'api/path/all/'+warehouses.startWHId +"/"+warehouses.destinationWHId;
        if(this.urlOrigin.includes("azure")){
          url = 'https://auth57.azurewebsites.net/api/path/all'+warehouses.startWHId +"/"+warehouses.destinationWHId;
        }
        let test : any[]=[];
        const data = warehouses
        const response= await this.sendFetch(url,'GET',null, document.cookie)
        console.log(response)
        if(response.status != 200){
         console.log("No paths found")
        }
        paths= await response.json();
        return paths;
    }

    async createPath(path:any){
        let url= this.urlOrigin+'api/path/'
        if(this.urlOrigin.includes("azure")){
          url = 'https://auth57.azurewebsites.net/api/path/';
        }
        const data = path;
        

      const response = await this.sendFetch(url,'POST',data, document.cookie);
      return response;
    }

    async createPathProlog(path:any){
      let url= this.urlOrigin+'api/path/prolog'
      if(this.urlOrigin.includes("azure")){
        url = 'https://auth57.azurewebsites.net/api/path/prolog';
      }
      const data = path;
      
      const response = await this.sendFetch(url,'POST',data, document.cookie);
      return response;
    }

    async sendFetch(url: string, method: string, data: any, cookie:any) {
        if(data)
          return fetch(url, {
            method: method,
            body: JSON.stringify(data),
            headers: {
              'Content-Type': 'application/json',
              "authorization": cookie,
            },
          })
        else
          return fetch(url, {
            method: method,
            headers: {
              'Accept': 'application/json',
              "authorization": cookie,
            }
          })
    }
}