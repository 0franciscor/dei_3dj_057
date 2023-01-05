import { TestBed } from '@angular/core/testing';

import { PlanningService} from './planning-service.service';

describe('PlanningService', () => {
  let service: PlanningService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PlanningService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=123');
    
  });

  it('cookie without jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('abc=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=');

  });

  // async getBestPath(TruckName: any, date: any){
  //   let url = this.urlOrigin+'api/planning/bestPath'
  //   if(this.urlOrigin.includes("azure")){
  //     url = 'https://auth57.azurewebsites.net/api/packaging/all';
  //   }
  //   const data={
  //     truck: TruckName,
  //     date: date, 
  //   }


  //   const response = await this.sendFetch(url,'POST',data, this.getJwt());
  //   const pathlist=await response.json();


   
  //     const url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
  //     const body= {pathList:pathlist.bestPath, date:date}
  //     const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
    
    
  //   return plan 
  // }

  it('should get best path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getBestPath("truck", "date");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getBestPath("truck", "date");
  });

  it('should get highest mass first path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getHighestMassFirst("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getHighestMassFirst("truck");
  });

  it('should get closest warehouse path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getClosestWarehouse("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getClosestWarehouse("truck");

  });

  it('should get cheapest path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getCheapestPath("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getCheapestPath("truck");

  });

  it('should get genetic algorithm path', async () => {
  
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getGeneticAlgorithm("2022125");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getGeneticAlgorithm("2022125");


  
  });


  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });
});
