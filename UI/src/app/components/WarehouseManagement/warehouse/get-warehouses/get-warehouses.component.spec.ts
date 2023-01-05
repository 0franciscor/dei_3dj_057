import { ComponentFixture, TestBed } from '@angular/core/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

import { GetWarehousesComponent } from './get-warehouses.component';

describe('GetWarehousesComponent', () => {
  let component: GetWarehousesComponent;
  let fixture: ComponentFixture<GetWarehousesComponent>;
  let fakeWarehouseService: any;
  let fakeLoginService: LoginService;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetWarehousesComponent ],
      providers: [WarehouseService]
    })
    .compileComponents();
    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['getAllWarehouses','activateWarehouse','deactivateWarehouse']);
    fakeWarehouseService.getAllWarehouses.and.returnValue(Promise.resolve({status: 200}));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(GetWarehousesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should go to Edit Warehouse', () => {

    component.goToEditWarehouse("TH1");
    expect(component).toBeTruthy();
  });

  it('should go to Active Warehouse', () => {

    component.goToActiveWarehouse("TH1");
    expect(component).toBeTruthy();
  });

  it('should go to Deactive Warehouse', () => {

    component.goToDeactiveWarehouse("TH1");
    expect(component).toBeTruthy();
  });
});

describe('LoginService', () => {
  let service: LoginService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(LoginService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('get role with jwt cookie', async () => {
    const response = {
      "status": 200,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  
  it('get role with null jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('');
    const role = await service.getRole();
    expect(fetchSpy).not.toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('get invalid role with jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('should login', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.login("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.login("test");

  });

  it('should login with google', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.loginWithGoogle("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.loginWithGoogle("test");

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

describe('WarehouseService', () => {
  let service: WarehouseService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(WarehouseService);
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

  it('should get a warehouse', async () => {
    const response = {
        "id": "TH1",
        "address": "Rua António Bernardino,47,4535-334,Porto",
        "altitude": 250,
        "latitude": "40.9321º N",
        "longitude": "8.2451º W",
        "designation": "Arouca",
        "city": "1",
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const warehouse = await service.getWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(warehouse).toEqual(response);
    service.urlOrigin = "https://azure:4200";
    await service.getWarehouse('TH1');
  });


  it('should create a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.createWarehouse('TH1');
  });

  it('should create a warehouse prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createWarehouseProlog('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createWarehouseProlog('TH1');

  });

  it('should update a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateWarehouse('TH1');
  });

  it('should update a warehouse prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateWarehouseProlog('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateWarehouseProlog('TH1');
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

  it('should activate a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.activateWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.activateWarehouse('TH1');
  });

  it('should deactivate a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deactivateWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deactivateWarehouse('TH1');
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
