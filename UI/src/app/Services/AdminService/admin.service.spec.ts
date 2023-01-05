import { TestBed } from '@angular/core/testing';

import { AdminService } from './admin.service';

describe('AdminService', () => {
  let service: AdminService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(AdminService);
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

  it('should create a user', async () => {
    const response = {
      "status": 201,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.createUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.createUser("data");
  });

  it('should create a user error', async () => {
    const response = {
      "status": 401,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.createUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.createUser("data");
  });

  it('should get all user', async () => {

    const response = {
      "status": 200,
      json() {
        return [{firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllUsers();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllUsers();


  });

  it('should not get all user', async () => {

    const response = {
      "status": 401,
      json() {
        return [{firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllUsers();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllUsers();


  });

  it('should get a user', async () => {

    const response = {
      "status": 200,
      json() {
        return {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getUser();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getUser();


  });

  it('should not get a user', async () => {

    const response = {
      "status": 401,
      json() {
        return {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getUser();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getUser();


  });

  it('should update a user', async () => {
  
    const response = {
      "status": 200,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.updateUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.updateUser("data");

  });

  it('should get all role', async () => {

    const response = {
      "status": 200,
      json() {
        return [{role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllRole();


  });

  it('should not get all role', async () => {

    const response = {
      "status": 401,
      json() {
        return [{role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllRole();


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
