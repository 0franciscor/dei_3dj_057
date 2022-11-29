import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {

  constructor() { }

  async getDeliveries(){
    const url = "http://localhost:3001/api/delivery/all";
    const urlBackUp = "http://5.249.66.111:3001/api/delivery/all";
    
    try{
      const response = await fetch(url, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });

      const data = await response.json();
      return data;  
    }
    catch{
      const response = await fetch(urlBackUp, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });
      const data = await response.json();
      return data;
    }
    
    
  }

  async getDelivery(deliveryID: string){
    const url = "http://localhost:3001/api/delivery/" + deliveryID;
    const urlBackUp = "http://5.249.66.111:3001/api/delivery/"+deliveryID;
    try{
      const response = await fetch(url, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });
      const data = await response.json();
      return data;
    }catch{
      const response = await fetch(urlBackUp, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });
      const data = await response.json();
      return data;
    }
    
  }

  async createDelivery(delivery: any) {
    const url = "http://localhost:3001/api/delivery/create";
    const urlBackUp = "http://5.249.66.111:3001/api/delivery/create";
    const data = delivery;
    try{

      const response = await fetch(url, {
        method: 'POST',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      return response;
    }catch{
      
      const response = await fetch(urlBackUp, {
        method: 'POST',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      return response;
    }
  }

  async updateDelivery(delivery: any) {
    const url = "http://localhost:3001/api/delivery/update";
    const urlBackUp = "http://5.249.66.111:3001/api/delivery/update";

    const data = delivery;
    try{
      const response = await fetch(url, {
        method: 'PATCH',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      return response;
    }catch{
      const response = await fetch(urlBackUp, {
        method: 'PATCH',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      return response;
    }
    
  }
}
