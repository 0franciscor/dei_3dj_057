import { ComponentFactoryResolver, Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {

  constructor() { }

  async getDeliveries(){
    const url = "http://localhost:3001/api/delivery/all";
    let receivedDelivery: any[] = [];
    await fetch(url, {
      method: 'GET'
    }).then(res => res.json().then(data => {receivedDelivery = data;}));
    console.log(receivedDelivery);
    return receivedDelivery;
  }

  async createDelivery(delivery: any) {
    const url = "http://localhost:3001/api/delivery/create";
    const data = delivery;
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
  }
}
