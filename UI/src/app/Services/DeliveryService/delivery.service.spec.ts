import { TestBed } from '@angular/core/testing';

import { DeliveryService } from './delivery.service';

describe('DeliveryService', () => {
  let service: DeliveryService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(DeliveryService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should get a delivery', async () => {
    const response = {
      "deliveryID": "123",
      "deliveryDate": "2023-05-05",
      "loadTime": 5,
      "unloadTime": 5,
      "destination": "1",
      "deliveryMass": 5,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.getDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery).toEqual(response);
  });

  it('should get all deliveries', async () => {
    const response = {
      "deliveries": [
        {
          "deliveryID": "123",
          "deliveryDate": "2023-05-05",
          "loadTime": 5,
          "unloadTime": 5,
          "destination": "1",
          "deliveryMass": 5
        }
      ],
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const deliveries = await service.getDeliveries();
    expect(fetchSpy).toHaveBeenCalled();
    expect(deliveries).toEqual(response);
  });

  it('should Create a delivery', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.createDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(201);
  });

  it('should Create a delivery prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.createDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(201);
  });

  it('should Update a delivery', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.updateDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
  });

  it('should Update a delivery prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.updateDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
  });

  it('should send a fetch without data', async () => {
    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', 'null', "cookie");
    expect(status.status).toEqual(404);
  });
});
