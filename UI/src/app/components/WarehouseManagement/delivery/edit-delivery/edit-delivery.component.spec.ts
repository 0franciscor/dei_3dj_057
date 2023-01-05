import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormControl, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { EditDeliveryComponent, EditDeliveryComponentDialog } from './edit-delivery.component';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { ActivatedRoute } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

describe('EditDeliveryComponent', () => {
  let component: EditDeliveryComponent;
  let fixture: ComponentFixture<EditDeliveryComponent>;
  let dialogComponent: EditDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<EditDeliveryComponentDialog>;
  let fakeDeliveryService: any;
  let fakeLoginService: LoginService;
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [EditDeliveryComponent, EditDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule, RouterTestingModule.withRoutes(
        [{ path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: '' }],
      )],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }, {
        provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => '1'
            }
          }
        }
      }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['updateDelivery', 'getDelivery', 'updateDeliveryProlog']);
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 200 }));

    fakeDeliveryService.getDelivery.and.returnValue(Promise.resolve({
      deliveryID: "123",
      deliveryDate: "2023-05-05",
      loadTime: 5,
      unloadTime: 5,
      destination: "1",
      deliveryMass: 5
    }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(EditDeliveryComponent);
    component = fixture.componentInstance;
    let fb = new FormBuilder();
    component.formEditDelivery = fb.group({
      deliveryID: new FormControl('', [Validators.required]),
      deliveryDate: new FormControl('', [Validators.required]),
      loadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      unloadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      destination: new FormControl('', [Validators.required]),
      deliveryMass: new FormControl('', [Validators.required, Validators.min(0)]),
    });
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(EditDeliveryComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
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

  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onOk', async () => {
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');

    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });

  it('component finishes on submit', async () => {
    await component.onSubmit();
  });

  it('onGoBack', async () => {
    component.goBack();
  });

  it('should call onSubmit', async () => {
    spyOn(component, 'onSubmit');
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });

  it('should call onSubmit with DeliveryID', async () => {
    spyOn(component, 'onSubmit');
    component.formEditDelivery.controls['deliveryID'].setValue("123");
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });

  it('should call onSubmit with error', async () => {
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 500 }));
    await component.onSubmit();
  });
});

describe('EditDeliveryComponent with null activated route', () => {
  let component: EditDeliveryComponent;
  let fixture: ComponentFixture<EditDeliveryComponent>;
  let dialogComponent: EditDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<EditDeliveryComponentDialog>;
  let fakeDeliveryService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [EditDeliveryComponent, EditDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule, RouterTestingModule.withRoutes(
        [{ path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: '' }],
      )],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }, {
        provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => null
            }
          }
        }
      }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['updateDelivery', 'getDelivery', 'updateDeliveryProlog']);
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 200 }));

    fakeDeliveryService.getDelivery.and.returnValue(Promise.resolve({
      deliveryID: "123",
      deliveryDate: "2023-05-05",
      loadTime: 5,
      unloadTime: 5,
      destination: "1",
      deliveryMass: 5
    }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fixture = TestBed.createComponent(EditDeliveryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    let fb = new FormBuilder();
    component.formEditDelivery = fb.group({
      deliveryID: new FormControl('', [Validators.required]),
      deliveryDate: new FormControl('', [Validators.required]),
      loadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      unloadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      destination: new FormControl('', [Validators.required]),
      deliveryMass: new FormControl('', [Validators.required, Validators.min(0)]),
    });
    dialogFixture = TestBed.createComponent(EditDeliveryComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
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


