import { Injectable } from '@angular/core';
import { SERVER_TOKEN } from '@angular/flex-layout';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
   
  constructor() {}

  async getTruck(truckID:string) {
    const url = this.urlOrigin+'api/truck/id/'+truckID;
  
    const response = await this.sendFetch(url, 'GET', null);
    const data = await response.json();

    return data;
    
   
  }
  
  async getAllTruck() {
    const url = this.urlOrigin+'api/truck/all';
    const response = await this.sendFetch(url, 'GET', null);
    const data = await response.json();
    return data;
    
  }

  async createTruck(truck: any) {
    const url = this.urlOrigin+'api/truck/';
    

    const data = truck;
 
    const response = await this.sendFetch(url, 'POST', data);
    return response;
    

  }

  async updateTruck(truck: any) {

    const url = this.urlOrigin+'api/truck/';

    const data = truck;
    const response = await this.sendFetch(url, 'PATCH', data);
      
    
    return response;
  
  }

  async deleteTruck(truckID: string) {
    const url = this.urlOrigin+'api/truck/id/'+truckID;

    const response = await this.sendFetch(url, 'DELETE', null);

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
