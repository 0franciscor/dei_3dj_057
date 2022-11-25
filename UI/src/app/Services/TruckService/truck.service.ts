import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {
  

  constructor() { }

  async getTruck(truckID:string) {
    const url = 'http://localhost:3001/api/truck/id/'+truckID;
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
    const url = 'http://localhost:3001/api/truck/all';
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
    const url = 'http://localhost:3001/api/truck/';
    const data = truck;
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    const info = await response.json();
    return info;

  }

  async updateTruck(truck: any) {

    const url = 'http://localhost:3001/api/truck/';
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
    const url = 'http://localhost:3001/api/truck/id/'+truckID;
    const response = await fetch(url, {
      method: 'DELETE',
      headers: {
        'Accept': 'application/json'
      }
    });
    

    return response;
  }


}
