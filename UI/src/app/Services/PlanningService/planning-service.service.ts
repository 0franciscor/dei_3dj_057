import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class PlanningService{
  
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

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

  async getBestPath(TruckName: any, date: any){
    let url = this.urlOrigin+'api/planning/bestPath'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/planning/bestPath';
    }
    const data={
      truck: TruckName,
      date: date, 
    }


    const response = await this.sendFetch(url,'POST',data, this.getJwt());
    const pathlist=await response.json();


   
    const url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
    const body= {pathList:pathlist.bestPath, date:date}
    const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
    
    
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


    const response = await this.sendFetch(url,'POST',data, this.getJwt());
    const pathlist=await response.json();


   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url2 = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
    
    
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

    const response = await this.sendFetch(url,'POST',data, this.getJwt());
    const pathlist=await response.json();


   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
    
    
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
    const response = await this.sendFetch(url,'POST',data, this.getJwt());
    const pathlist=await response.json();


   
      let url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      if(this.urlOrigin.includes("azure")){
        url2 = 'https://auth57.azurewebsites.net/api/delivery/getDeliveryDestination';
      }
      const body= {pathList:pathlist.bestRoute, date:date}
      const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
    
  
    return plan 
  }

  async getGeneticAlgorithm(date: any){
    let url = this.urlOrigin+'api/planning/geneticAlgorithm'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/planning/geneticAlgorithm';
    }
    const data={
      date: date, 
    }


    const response = await this.sendFetch(url,'POST',data, this.getJwt());
    const pathList = await response.json();

    return pathList
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
