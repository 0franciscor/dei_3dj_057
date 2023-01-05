import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA  } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { CreatePathComponent, CreatePathComponentDialog } from './create-path.component';
import {MatCardModule} from '@angular/material/card';
import {MatFormFieldModule} from '@angular/material/form-field';
import {MatInputModule} from '@angular/material/input';
import { PathService } from 'src/app/Services/PathService/path.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';


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
