import { TestBed } from '@angular/core/testing';

import { RoadNetworkService } from './road-network.service';

describe('RoadNetworkService', () => {
  let service: RoadNetworkService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(RoadNetworkService);
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

  it('should return path between warehouses', async () => {
    const response = {
      "path": [
        {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        },
        {
          "id": "TH2",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        }
      ],
      json () {
        return this;
      },
      status: 200
    };


    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const path = await service.getPathBetweenWarehouses("WH1");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path).toEqual(response);
    service.urlOrigin = "https://azure:4200";
    await service.getPathBetweenWarehouses("WH1");
  });
  it('should return null on getPathBetweenWarehouses', async () => {
    

    const response2 = {
      "path": [],
      json () {
        return this;
      },
      status: 404
    };

    const fetchSpy2 = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response2));

    const path2 = await service.getPathBetweenWarehouses("WH1");
    expect(fetchSpy2).toHaveBeenCalled();
    expect(path2).toEqual(null);
    service.urlOrigin = "https://azure:4200";
    await service.getPathBetweenWarehouses("WH1");
  });

  it('should get all trucks', async () => {
    const response = {
      "trucks": [
        {
          "truckID": "test",
          "tare": 1,
          "capacity": 1,
          "maxBatteryCapacity": 1,
          "autonomy": 1,
          "fastChargeTime": 1,
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllTrucks();
    expect(fetchSpy).toHaveBeenCalled();
    expect(trucks).toEqual(response);
    service.urlOrigin = "https://azure:4200";
    await service.getAllTrucks();
  });

  it('should get all warehouses', async () => {
    const response = {
      "warehouses": [
        {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllWarehouses();
    expect(fetchSpy).toHaveBeenCalled();
    expect(trucks).toEqual(response);
    service.urlOrigin = "https://azure:4200";
    await service.getAllWarehouses();
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
