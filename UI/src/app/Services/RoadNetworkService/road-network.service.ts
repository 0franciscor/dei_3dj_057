import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class RoadNetworkService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() {}

  getJwt() {
    const cookies = document.cookie.split(';');
    
    let jwt = "";
    for (const cookie of cookies) {
      const [name, value] = cookie.split('=');
      if(name.trim() === "jwt"){
        jwt = value;
      }
    }
    const cookie = "jwt=" + jwt;
    return cookie;
  }

  async sendFetch(url: string, method: string, data: any, cookie: any) {
    if(data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          "authorization": cookie,
        },
      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json',
          "authorization": cookie,
        }
      })
  }


  async getAllWarehouses() {
    let url = this.urlOrigin+"api/warehouse/all";
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/all';
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());
    
    let data = await response.json();
    return data;
  }

  async getAllTrucks() {

    let url = this.urlOrigin+"api/truck/all";
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/all';
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());

    let data = await response.json();
    return data;

  }
  

  async getPathBetweenWarehouses(warehouse1: string) {

    let url = this.urlOrigin+"api/path/all/" + warehouse1+"/undefined";
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/path/all/'+warehouse1+"/undefined";
    }
    
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());
    
    if(response.status == 404){
      return null;
    }else{
      
      let data = await response.json();
      
      return data;

    }

    

  }
}