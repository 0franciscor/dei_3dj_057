import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  private cookieName = "jwt";
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

  async getTruck(truckID:string) {
    let url = this.urlOrigin+'api/truck/id/'+truckID;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/id/'+truckID;
    }
    const response = await this.sendFetch(url, 'GET', null, document.cookie);
    const data = await response.json();

    return data;
    
   
  }
  
  async getAllTruck() {
    let url = this.urlOrigin+'api/truck/all';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/all';
    }
    

    const response = await this.sendFetch(url, 'GET', null, this.getJwt());
    const data = await response.json();
    return data;
    
  }

  async createTruck(truck: any) {
    let url = this.urlOrigin+'api/truck/';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/';
    }

    const data = truck;
 
    const response = await this.sendFetch(url, 'POST', data, this.getJwt());
    console.log("response", response);
    return response;
    

  }

  async createTruckProlog(truck: any) {
    let url = this.urlOrigin+'api/truck/prolog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/prolog';
    }

    const data = truck;
 
    const response = await this.sendFetch(url, 'POST', data, this.getJwt());
    return response;
    

  }

  async updateTruck(truck: any) {

    let url = this.urlOrigin+'api/truck/';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/';
    }
    const data = truck;
    const response = await this.sendFetch(url, 'PATCH', data, this.getJwt());
      
    
    return response;
  
  }

  async updateTruckProlog(truck: any) {

    let url = this.urlOrigin+'api/truck/prolog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/prolog';
    }
    const data = truck;
    const response = await this.sendFetch(url, 'PATCH', data, this.getJwt());
      
    
    return response;
  
  }

  async toggleActiveTruck(truckID: string) {
    let url = this.urlOrigin+'api/truck/id/'+truckID;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/id/'+truckID;
    }
    const response = await this.sendFetch(url, 'DELETE', null, this.getJwt());

    return response;
  }

  async deleteTruckProlog(truckID: string) {
    let url = this.urlOrigin+'api/truck/idProlog/'+truckID;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/truck/idProlog/'+truckID;
    }
    // const response = await this.sendFetch(url, 'DELETE', null, this.getJwt());
    const response = {
      status: 200
    }
    return response;
  }

  async sendFetch(url: string, method: string, data: any, cookie: any) {
    if(data)
    //send cookie with request
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


}
