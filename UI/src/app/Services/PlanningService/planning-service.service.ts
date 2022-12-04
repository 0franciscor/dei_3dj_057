import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class PlanningService{
  
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  async getBestPath(TruckName: any, date: any){
    const url = this.urlOrigin+'api/planning/bestPath'
    const data={
      truck: TruckName,
      date: date, 
    }
    console.log(data)

    const response = await this.sendFetch(url,'POST',data);
    const pathlist=await response.json();
    console.log(pathlist.bestPath[1])

   
      const url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
      const body= {pathList:pathlist.bestPath, date:date}
      const plan = await this.sendFetch(url2,'POST',body)
    
    
    return plan 
  }

  async getHighestMassFirst(date: any){
    const url = this.urlOrigin+'api/planning/heuristicMass'
    const data={
      date: date,
    }

    const response = await this.sendFetch(url,'POST',data);
    return response 
  }

  async getClosestWarehouse(date: any){
    const url = this.urlOrigin+'api/planning/heuristicClosestWarehouse'
    const data={
      date: date,
    }

    const response = await this.sendFetch(url,'POST',data);
    return response 
  }

  async getCheapestPath(date: any){
    const url = this.urlOrigin+'api/planning/heuristicMassAndDistance'
    const data={
      date: date,
    }

    const response = await this.sendFetch(url,'POST',data);
    return response 
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
