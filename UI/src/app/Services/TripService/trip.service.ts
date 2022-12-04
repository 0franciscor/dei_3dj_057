import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class TripService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
   
  constructor() {}

  async createTrip(savePlan: any) {
    const url = this.urlOrigin+'api/trip/';
  
    const response = await this.sendFetch(url, 'POST', savePlan);
    const data = await response.json();

    return data;
    
   


  }

  async sendFetch(url: string, method: string, data: any) {
    if(data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json'
        }
      })
  }
}
