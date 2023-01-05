import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormControl, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

import { EditWarehouseComponent, EditWarehouseComponentDialog } from './edit-warehouse.component';

describe('EditWarehouseComponent', () => {
  let component: EditWarehouseComponent;
  let fixture: ComponentFixture<EditWarehouseComponent>;
  let dialogComponent: EditWarehouseComponentDialog;
  let dialogFixture: ComponentFixture<EditWarehouseComponentDialog>;
  let fakeWarehouseService: any;
  let fakeLoginService: LoginService;
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditWarehouseComponent, EditWarehouseComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: ''}]
      )],
      providers: [WarehouseService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => '1'
            }
          }
        }}]
      
    })
    .compileComponents();

    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['updateWarehouse', 'getWarehouse','updateWarehouseProlog']);
    fakeWarehouseService.updateWarehouse.and.returnValue(Promise.resolve({status: 200}));

    fakeWarehouseService.getWarehouse.and.returnValue(Promise.resolve({
        id: "TH1",
        address: "Rua António Bernardino,47,4535-334,Porto",
        altitude: 250,
        latitude: "40.9321º N",
        longitude: "8.2451º W",
        designation: "Arouca",
        city: 1
      }));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(EditWarehouseComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    let selectedWarehouse = {
      id: "",
      address: undefined,
      altitude: undefined,
      latitude: undefined,
      longitude: undefined,
      designation: undefined,
    }
    let fb = new FormBuilder();
    component.formEditWarehouse = fb.group({
      Id: new FormControl('', [Validators.required, Validators.minLength(3),Validators.maxLength(3)]),
      Address:new FormControl('', [Validators.required]),
      Altitude:new FormControl('', [Validators.required, Validators.min(0),Validators.max(13000)]),
      Latitude:new FormControl('', [Validators.required,Validators.maxLength(11)]),
      Longitude:new FormControl('', [Validators.required,Validators.maxLength(12)]),
      Designation:new FormControl('', [Validators.required, Validators.maxLength(50)])
    });
    
    dialogFixture = TestBed.createComponent(EditWarehouseComponentDialog);
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


  it('component finishes on Submit', async () => {
    component.formEditWarehouse.controls['Id'].setValue('TH1');
    component.formEditWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formEditWarehouse.controls['Altitude'].setValue(200);
    component.formEditWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formEditWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formEditWarehouse.controls['Designation'].setValue('Porto');
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


 
  it('should call onSubmit with WarehouseID', async () => {
    spyOn(component, 'onSubmit');
    component.formEditWarehouse.controls['Id'].setValue('id');
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });


  it('should call onSubmit with error', async () => {
    fakeWarehouseService.updateWarehouse.and.returnValue(Promise.resolve({status: 500}));
    await component.onSubmit();
  });
});

describe('EditWarehouseComponent with null activated route', () => {
  let component: EditWarehouseComponent;
  let fixture: ComponentFixture<EditWarehouseComponent>;
  let dialogComponent: EditWarehouseComponentDialog;
  let dialogFixture: ComponentFixture<EditWarehouseComponentDialog>;
  let fakeWarehouseService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditWarehouseComponent, EditWarehouseComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: ''}]
      )],
      providers: [WarehouseService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => null
            }
          }
        }}]
      
    })
    .compileComponents();

    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['updateWarehouse', 'getWarehouse','updateWarehouseProlog']);
    fakeWarehouseService.updateWarehouse.and.returnValue(Promise.resolve({status: 200}));

    fakeWarehouseService.getWarehouse.and.returnValue(Promise.resolve({
        id: "TH1",
        address: "Rua António Bernardino,47,4535-334,Porto",
        altitude: 250,
        latitude: "40.9321º N",
        longitude: "8.2451º W",
        designation: "Arouca",
        city: "1"
      }));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fixture = TestBed.createComponent(EditWarehouseComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    
    dialogFixture = TestBed.createComponent(EditWarehouseComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();

  });

  it('should create', () => {
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