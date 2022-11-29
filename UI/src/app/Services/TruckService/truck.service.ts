import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
   
  constructor() {}

  async getTruck(truckID:string) {
    const url = this.urlOrigin+'api/truck/id/'+truckID;
  
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();

    return data;
    
   
  }
  
  async getAllTruck() {
    const url = this.urlOrigin+'api/truck/all';
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });
    const data = await response.json();
    return data;
    
  }

  async createTruck(truck: any) {
    const url = this.urlOrigin+'api/truck/';
    

    const data = truck;
 
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
    

  }

  async updateTruck(truck: any) {

    const url = this.urlOrigin+'api/truck/';

    const data = truck;
    const response = await fetch(url, {
      method: 'PATCH',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
      
    
    return response;
  
  }

  async deleteTruck(truckID: string) {
    const url = this.urlOrigin+'api/truck/id/'+truckID;
       
    const response = await fetch(url, {
      method: 'DELETE',
      headers: {
        'Accept': 'application/json'
      }
    });
    

    return response;
   
  }


}
