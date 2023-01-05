import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormControl, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { CreateDeliveryComponent } from './create-delivery.component';
import { CreateDeliveryComponentDialog } from './create-delivery.component';
import { MatDialogRef, MatDialogModule, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { LoginService } from 'src/app/Services/LoginService/login.service';

describe('CreateDeliveryComponent', () => {
  let component: CreateDeliveryComponent;
  let fixture: ComponentFixture<CreateDeliveryComponent>;
  let dialogComponent: CreateDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<CreateDeliveryComponentDialog>;
  let fakeDeliveryService: any;
  let fakeLoginService: LoginService
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CreateDeliveryComponent, CreateDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['createDelivery', 'createDeliveryProlog']);
    fakeDeliveryService.createDelivery.and.returnValue(Promise.resolve({ status: 201 }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(CreateDeliveryComponent);
    component = fixture.componentInstance;
    let fb = new FormBuilder();
    component.formCreateDelivery = fb.group({
      deliveryID: new FormControl('', [Validators.required]),
      deliveryDate: new FormControl('', [Validators.required]),
      loadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      unloadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      destination: new FormControl('', [Validators.required]),
      deliveryMass: new FormControl('', [Validators.required, Validators.min(0)]),
    });
    fixture.detectChanges();
    component.ngOnInit();
    
    dialogFixture = TestBed.createComponent(CreateDeliveryComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
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

  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onSubmit with invalid form', async () => {
    await component.onSubmit();
    expect(component.formCreateDelivery.valid).toBeFalsy();
  });

  it('onSubmit with valid form', async () => {
    component.formCreateDelivery.controls['deliveryID'].setValue("123");
    component.formCreateDelivery.controls['deliveryDate'].setValue("2023-05-05");
    component.formCreateDelivery.controls['loadTime'].setValue(5);
    component.formCreateDelivery.controls['unloadTime'].setValue(5);
    component.formCreateDelivery.controls['destination'].setValue("1");
    component.formCreateDelivery.controls['deliveryMass'].setValue(5);
    await component.onSubmit();
    expect(component.formCreateDelivery.valid).toBeTruthy();
  });

  it('onSubmit with valid form, error on create', async () => {
    component.formCreateDelivery.controls['deliveryID'].setValue("123");
    component.formCreateDelivery.controls['deliveryDate'].setValue("2023-05-05");
    component.formCreateDelivery.controls['loadTime'].setValue(5);
    component.formCreateDelivery.controls['unloadTime'].setValue(5);
    component.formCreateDelivery.controls['destination'].setValue("1");
    component.formCreateDelivery.controls['deliveryMass'].setValue(1);
    fakeDeliveryService.createDelivery.and.returnValue(Promise.resolve({ status: 500 }));
    await component.onSubmit();
    expect(component.deliveryID?.value).toBe("123");
    expect(component.deliveryDate?.value).toBe("2023-05-05");
    expect(component.loadTime?.value).toBe(5);
    expect(component.unloadTime?.value).toBe(5);
    expect(component.destination?.value).toBe("1");
    expect(component.deliveryMass?.value).toBe(1);
    
    expect(component.formCreateDelivery.valid).toBeTruthy();
  });

  it('onOk', async () => {
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
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
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.createDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createDeliveryProlog("123");
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
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.updateDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateDeliveryProlog("123");
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