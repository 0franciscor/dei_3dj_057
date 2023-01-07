import { Component } from '@angular/core';
import { TestBed } from '@angular/core/testing';
import { combineLatest } from 'rxjs';

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
    service.urlOrigin = "https://azure:4200";
    await service.getDelivery("test");
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
    service.urlOrigin = "https://azure:4200";
    await service.getDeliveries();
  });

  it('should Create a delivery', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.createDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createDelivery("123");
  });

  it('should Create a delivery prolog', async () => {
    const date: Date = new Date("2023-05-05");
    const response = {
      "status": 201,
    };
    const deliveryParam = {
      "deliveryID": "1234",
      "deliveryDate": date,
      "loadTime": 5,
      "unloadTime": 5,
      "destination": "1",
      "deliveryMass": 5
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.createDeliveryProlog(deliveryParam);
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createDeliveryProlog(deliveryParam);
  });

  it('should Update a delivery', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.updateDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateDelivery("123");
  });

  it('should Update a delivery prolog', async () => {
    const date: Date = new Date("2023-05-05");
    const response = {
      "deliveries": [
        {
          "deliveryID": "123",
          "deliveryDateProlog": date,
          "loadTime": 5,
          "unloadTime": 5,
          "destination": "1",
          "deliveryMass": 5
        }
      ],
      json() {
        return this;
      },
      "status": 200,
    };
    const deliveryParam = {
      "deliveryID": "1234",
      "deliveryDate": date,
      "loadTime": 5,
      "unloadTime": 5,
      "destination": "1",
      "deliveryMass": 5
    }

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.updateDeliveryProlog(deliveryParam);
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateDeliveryProlog(deliveryParam);
  });

  it('should send a fetch without data', async () => {
    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', 'null', "cookie");
    expect(status.status).toEqual(404);
  });

  it('should delete delivery', async () => {

    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.deleteDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteDelivery("123");

  });

  it('should delete delivery prolog', async () => {

    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.deleteDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteDeliveryProlog("123");

  });

});
