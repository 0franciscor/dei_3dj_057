import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, NgControl, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { GetDeliveriesComponent } from './get-deliveries.component';

describe('GetDeliveriesComponent', () => {
  let component: GetDeliveriesComponent;
  let fixture: ComponentFixture<GetDeliveriesComponent>;
  let fakeDeliveryService: any;
  let fakeLoginService: LoginService;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [GetDeliveriesComponent],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: {} }, { provide: MAT_DIALOG_DATA, useValue: {} }]

    })
      .compileComponents();
      // public string deliveryID { get; set; }
      //   public DateTime deliveryDate { get; set; }
      //   public float loadTime { get; set; }
      //   public float unloadTime { get; set; }
      //   public string destination { get; set; }
      //   public float deliveryMass { get; set; }

      //   public string deliveryDateProlog {get; set;}
    let deliveries = [
      {
        "deliveryID": "1",
        "deliveryDate": "2021-05-01T00:00:00",
        "loadTime": 1,
        "unloadTime": 1,
        "destination": "1",
        "deliveryMass": 1,
        "deliveryDateProlog": "2021-05-01T00:00:00"
      },
      {
        "deliveryID": "",
        "deliveryDate": "2021-05-01T00:00:00",
        "loadTime": 1,
        "unloadTime": 1,
        "destination": "1",
        "deliveryMass": 1,
        "deliveryDateProlog": "2021-05-01T00:00:00"
      },
    ]
    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['getDeliveries', 'deleteDelivery', 'deleteDeliveryProlog']);
    fakeDeliveryService.getDeliveries.and.returnValue(Promise.resolve(deliveries));
    fakeDeliveryService.deleteDelivery.and.returnValue(Promise.resolve({status:200}));
    fakeDeliveryService.deleteDeliveryProlog.and.returnValue(Promise.resolve({status:200}));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(GetDeliveriesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    component.ngOnInit();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should go to edit delivery', () => {
  
    component.goToEditDelivery("1");
    expect(component).toBeTruthy();
  
  
  });

  it('should delete delivery', async () => {

    const response = await component.deleteDelivery("1");
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