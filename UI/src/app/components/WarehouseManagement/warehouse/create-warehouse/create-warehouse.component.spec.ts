import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

import { CreateWarehouseComponent, CreateWarehouseComponentDialog } from './create-warehouse.component';

describe('CreateWarehouseComponent', () => {
  let component: CreateWarehouseComponent;
  let fixture: ComponentFixture<CreateWarehouseComponent>;
  let dialogComponent: CreateWarehouseComponentDialog;
  let dialogFixture: ComponentFixture<CreateWarehouseComponentDialog>;
  let fakeWarehouseService: any;
  let fakeLoginService: LoginService
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateWarehouseComponent,CreateWarehouseComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule,
        BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
        providers: [WarehouseService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['createWarehouse','createWarehouseProlog']);
    fakeWarehouseService.createWarehouse.and.returnValue(Promise.resolve({status: 200}));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(CreateWarehouseComponent);
    component = fixture.componentInstance;
    let fb = new FormBuilder();
    component.formCreateWarehouse = fb.group({
      Id: new FormControl('', [Validators.required, Validators.minLength(3),Validators.maxLength(3)]),
      Address:new FormControl('', [Validators.required]),
      Altitude:new FormControl('', [Validators.required, Validators.min(0),Validators.max(13000)]),
      Latitude:new FormControl('', [Validators.required,Validators.maxLength(11)]),
      Longitude:new FormControl('', [Validators.required,Validators.maxLength(12)]),
      Designation:new FormControl('', [Validators.required, Validators.maxLength(50)])

    });
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(CreateWarehouseComponentDialog);
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
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    expect(component).toBeTruthy();
  });

  it('should create dialog', () => {
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    expect(dialogComponent).toBeTruthy();
  });

  it('onSubmit with invalid form', async () => {
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    await component.onSubmit();
    expect(component.formCreateWarehouse.valid).toBeFalsy();
  });


  it('onSubmit with valid form', async () => {

    component.formCreateWarehouse = new FormGroup({
      Id: new FormControl(''),
      Address:new FormControl(''),
      Altitude:new FormControl(''),
      Latitude:new FormControl(''),
      Longitude:new FormControl(''),
      Designation:new FormControl('')
    });

    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    
    await component.onSubmit();
    expect(component.Id?.value).toEqual('TH1');
    expect(component.Address?.value).toEqual('Rua António Bernardino,47,4535-334,Porto');
    expect(component.Altitude?.value).toEqual(200);
    expect(component.Latitude?.value).toEqual('40.9321º N');
    expect(component.Longitude?.value).toEqual('8.2451º W');
    expect(component.Designation?.value).toEqual('Porto');

    expect(component.formCreateWarehouse.valid).toBeTruthy();
  });


  it('onSubmit with valid form, error on create', async () => {
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    fakeWarehouseService.createWarehouse.and.returnValue(Promise.resolve({status: 500}));
    await component.onSubmit();
    expect(component.formCreateWarehouse.valid).toBeTruthy();
  });

  it('onOk', async () => {
    
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
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


