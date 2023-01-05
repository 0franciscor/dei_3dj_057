import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { PackagingService } from 'src/app/Services/PackageService/package.service';
import { PathService } from 'src/app/Services/PathService/path.service';

import { CreatePathComponent, CreatePathComponentDialog } from '../../path/create-path/create-path.component';
import { LogisticsManagerComponent } from './logistics-manager.component';

describe('LogisticsManagerComponent', () => {
  let component: LogisticsManagerComponent;
  let fixture: ComponentFixture<LogisticsManagerComponent>;
  let dialogComponent: CreatePathComponentDialog;
  let dialogFixture: ComponentFixture<CreatePathComponentDialog>;
  let fakePathService : any;
  let fakeLoginService : LoginService;
  const dialogMock = {
    close: () => { }
  };

  const pathList =[
   { id: '1',
    pathID:'teste1',
    startWHId :'testeWH1',
    destinationWHId: 'testeWH2',
    pathDistance :1,
    pathTravelTime: 1,
    wastedEnergy: 1,
    extraTravelTime: 1,
  },

  {
    id: '2',
    pathID:'teste2',
    startWHId :'testeWH3',
    destinationWHId: 'testeWH4',
    pathDistance :1,
    pathTravelTime: 1,
    wastedEnergy: 1,
    extraTravelTime: 1,
  }]

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LogisticsManagerComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Path/CreatePath', redirectTo: ''},
          {path: 'Logistics/RoadNetwork',redirectTo:''}
        ])],
        providers: [PathService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fakePathService = jasmine.createSpyObj('PathService',['getAllPaths']);
    fakePathService.getAllPaths.and.returnValue(Promise.resolve(pathList));

    TestBed.overrideProvider(PathService,{useValue:fakePathService});
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(LogisticsManagerComponent);
    component = fixture.componentInstance;
    let fb = new FormBuilder();
    component.formSelectWarehouse = fb.group({
      startWHId: [''],
      destinationWHId:['']
    });
    fixture.detectChanges();
    
    dialogFixture = TestBed.createComponent(CreatePathComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();

  });

  it('should create', () => {
    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to create Path',()=>{
    component.goToCreatePath();
    expect(component).toBeTruthy();
  });

  it('should go to create package',()=>{
    component.goToCreatePackage();
    expect(component).toBeTruthy();
  });

  it('should go to truck planning',()=>{
    component.goToTruckPlanning();
    expect(component).toBeTruthy();
  });

  it('should go to package list',()=>{

    component.goToPackageList();
    expect(component).toBeTruthy();

  });


  it('onOk', () => {
    dialogComponent.onOk();
    expect(dialogComponent).toBeTruthy();
  });
  
  it('onSubmit',()=>{
    component.onSubmit();
    expect(component).toBeTruthy();
  })



  it('goToRoadNetwork',()=>{
    component.goToRoadNetwork();
    expect(component).toBeTruthy();
  })
});

describe('PathService', ()=>{
  let service: PathService;

  beforeEach(() => {
      TestBed.configureTestingModule({});
      service = TestBed.inject(PathService);
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

    it('should get all paths', async () => {
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

      const warehouses = {
        startWHId: "WH1",
        destinationWHId: "WH2"
      }
      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
      const paths = await service.getAllPaths(warehouses);
      expect(fetchSpy).toHaveBeenCalled();
      expect(paths).toEqual(response);
      service.urlOrigin = "https://azure:4200";
      await service.getAllPaths(warehouses);

    });

    it('should get all paths with undefined', async () => {
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
        status: 404
      };

      const warehouses = {
        startWHId: "",
        destinationWHId: ""
      }
      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
      const paths = await service.getAllPaths(warehouses);
      expect(fetchSpy).toHaveBeenCalled();
      expect(paths).toEqual(response);
      service.urlOrigin = "https://azure:4200";
      await service.getAllPaths(warehouses);

    });

    it('should create a path', async () => {

      const response = {
        "path": {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        },
        json () {
          return this;
        },
        status: 200
      };

      const path = {
        "id": "TH1",
        "address": "Rua António Bernardino,47,4535-334,Porto",
        "altitude": 250,
        "latitude": "40.9321º N",
        "longitude": "8.2451º W",
        "designation": "Arouca",
        "city": "1",
      }

      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
      const paths = await service.createPath(path);
      expect(fetchSpy).toHaveBeenCalled();
      service.urlOrigin = "https://azure:4200";
      await service.createPath(path);


    });

    it('should create a path in prolog', async () => {

      const response = {
        "path": {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        },
        json () {
          return this;
        },
        status: 200
      };

      const path = {
        "id": "TH1",
        "address": "Rua António Bernardino,47,4535-334,Porto",
        "altitude": 250,
        "latitude": "40.9321º N",
        "longitude": "8.2451º W",
        "designation": "Arouca",
        "city": "1",
      }

      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
      const paths = await service.createPathProlog(path);
      expect(fetchSpy).toHaveBeenCalled();
      service.urlOrigin = "https://azure:4200";
      await service.createPathProlog(path);


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

describe('PackagingService', () => {
  let service: PackagingService;

  beforeEach(() => {
      TestBed.configureTestingModule({});
      service = TestBed.inject(PackagingService);      
  });

  it('should be created', () => {
      expect(service).toBeTruthy();
  })

  it('should be created', () => {
      expect(service).toBeTruthy();
  });
  
  it('cookie with jwt', () => {
      spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
      const cookie = service.getJwt();
      expect(cookie).toEqual('jwt=123');
      
  });

  it('should create a package', async () => {
      const response = {
        "status": 201,
        json() {
          return this;
        }
      };
  
      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
  
      const status = await service.createPackage('test');
      expect(fetchSpy).toHaveBeenCalled();
      expect(status.status).toEqual(201);
      service.urlOrigin = "https://azure:4200";
      await service.createPackage("test");
  });

  it('should get all packages', async () => {

      const response = {
        "status": 200,
        json() {
          return this;
        }
      };
  
      const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
  
      const status = await service.getPackage();
      expect(fetchSpy).toHaveBeenCalled();
      expect(status.status).toEqual(200);
      service.urlOrigin = "https://azure:4200";
      await service.getPackage();


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

describe('CreatePathComponent', () => {
  let component: CreatePathComponent;
  let fixture: ComponentFixture<CreatePathComponent>;
  let dialogComponent: CreatePathComponentDialog;
  let dialogFixture: ComponentFixture<CreatePathComponentDialog>;
  let fakePathService: any;
  let fakeLoginService: LoginService;
  const dialogMock={
    close:()=>{}
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreatePathComponent , CreatePathComponentDialog],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule,BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
      providers:[PathService,{provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} }]
    })
    .compileComponents();

    fakePathService = jasmine.createSpyObj('PathService', ['createPath','createPathProlog']);
    fakePathService.createPath.and.returnValue(Promise.resolve({status:201}));
    
    TestBed.overrideProvider(PathService,{useValue:fakePathService});
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(CreatePathComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    component.formCreatePath= new FormGroup({
      pathID: new FormControl('', [Validators.required]),
      startWHId: new FormControl('', [Validators.required]),
      destinationWHId: new FormControl('', [Validators.required]),
      pathDistance: new FormControl('', [Validators.required]),
      pathTravelTime: new FormControl('', [Validators.required]),
      wastedEnergy: new FormControl('', [Validators.required]),
      extraTravelTime:new FormControl('', [Validators.required])
    });

    dialogFixture = TestBed.createComponent(CreatePathComponentDialog);
    dialogComponent= dialogFixture.componentInstance;
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
    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onSubmit with valid form',async ()=>{
    component.formCreatePath.controls['pathID'].setValue('teste1');
    component.formCreatePath.controls['startWHId'].setValue('t1');
    component.formCreatePath.controls['destinationWHId'].setValue('t2');
    component.formCreatePath.controls['pathDistance'].setValue(1);
    component.formCreatePath.controls['pathTravelTime'].setValue(1);
    component.formCreatePath.controls['wastedEnergy'].setValue(1);
    component.formCreatePath.controls['extraTravelTime'].setValue(1);
    await component.onSubmit();
    expect(component.formCreatePath.valid).toBeTruthy();
  })

  it('onSubmit valid with error on create', async()=>{
    component.formCreatePath.controls['pathID'].setValue('teste1');
    component.formCreatePath.controls['startWHId'].setValue('t1');
    component.formCreatePath.controls['destinationWHId'].setValue('t2');
    component.formCreatePath.controls['pathDistance'].setValue(1);
    component.formCreatePath.controls['pathTravelTime'].setValue(1);
    component.formCreatePath.controls['wastedEnergy'].setValue(1);
    component.formCreatePath.controls['extraTravelTime'].setValue(1);
    fakePathService.createPath.and.returnValue(Promise.resolve({status:500}))
    await component.onSubmit();
    expect(component.formCreatePath.valid).toBeTruthy();
  })

  it('onOk',async()=>{
    const dialogSpy = spyOn(dialogComponent.dialogRef,'close');

    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });
});