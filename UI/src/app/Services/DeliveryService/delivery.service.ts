import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  async getDeliveries(){
    const url = this.urlOrigin+'api/delivery/all';
  
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();
    return data;  
    
    
  }

  async getDelivery(deliveryID: string){
    const url = this.urlOrigin+'api/delivery/' + deliveryID;
   
      const response = await fetch(url, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });
      const data = await response.json();
      return data;
    
    
  }

  async createDelivery(delivery: any) {
    const url = this.urlOrigin+'api/delivery/create';
    
    const data = delivery;
   
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
    
  }

  async createDeliveryProlog(delivery: any) {
    const url = this.urlOrigin+'api/delivery/createProlog';
    
    const data = delivery;
   
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
    
  }

  async updateDelivery(delivery: any) {
    const url = this.urlOrigin+'api/delivery/update';
   

    const data = delivery;
   
    const response = await fetch(url, {
      method: 'PATCH',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
    
    
  }

  async updateDeliveryProlog(delivery: any) {
    const url = this.urlOrigin+'api/delivery/updateProlog';

    const data = delivery;
   
    const response = await fetch(url, {
      method: 'PATCH',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
    
    
  }
}
