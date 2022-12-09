import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class PlanningService{
  
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  async getBestPath(TruckName: any, date: any){
    let url = this.urlOrigin+'api/planning/bestPath'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/packaging/all';
    }
    const data={
      truck: TruckName,
      date: date, 
    }
    console.log(data)

    const response = await this.sendFetch(url,'POST',data, document.cookie);
    const pathlist=await response.json();
    console.log(pathlist.bestPath[1])

   
      const url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      const body= {pathList:pathlist.bestPath, date:date}
      const plan = await this.sendFetch(url2,'POST',body, document.cookie)
    
    
    return plan 
  }

  async getHighestMassFirst(date: any){
    let url = this.urlOrigin+'api/planning/heuristicMass'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/planning/heursiticMass';
    }
    const data={
      date: date, 
    }
    console.log(data)

    const response = await this.sendFetch(url,'POST',data, document.cookie);
    const pathlist=await response.json();
    console.log(pathlist.bestRoute)

   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url2 = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, document.cookie)
    
    
    return plan 
  }

  async getClosestWarehouse(date: any){
    let url = this.urlOrigin+'api/planning/heuristicClosestWarehouse'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/planning/heuristicClosestWarehouse';
    }
    const data={
      date: date,
    }

    const response = await this.sendFetch(url,'POST',data, document.cookie);
    const pathlist=await response.json();
    console.log(pathlist)

   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, document.cookie)
    
    
    return plan 
  }

  async getCheapestPath(date: any){
    let url = this.urlOrigin+'api/planning/heuristicMassAndDistance'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/planning/heuristicMassAndDistance';
    }
    const data={
      date: date,
    }
    const response = await this.sendFetch(url,'POST',data, document.cookie);
    const pathlist=await response.json();
    console.log(pathlist)

   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url2 = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, document.cookie)
    
    
    return plan 
  }

  async sendFetch(url: string, method: string, data: any, cookie: any) {
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
