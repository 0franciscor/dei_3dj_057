import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA  } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { CreatePathComponent, CreatePathComponentDialog } from './create-path.component';
import {MatCardModule} from '@angular/material/card';
import {MatFormFieldModule} from '@angular/material/form-field';
import {MatInputModule} from '@angular/material/input';
import { PathService } from 'src/app/Services/PathService/path.service';


describe('CreatePathComponent', () => {
  let component: CreatePathComponent;
  let fixture: ComponentFixture<CreatePathComponent>;
  let dialogComponent: CreatePathComponentDialog;
  let dialogFixture: ComponentFixture<CreatePathComponentDialog>;
  let fakePathService: any;

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

  it('should create', () => {
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




describe('PathService',()=>{
  let service: PathService;

  beforeEach(()=>{
    TestBed.configureTestingModule({});
    service=TestBed.inject(PathService);
  })

  it('should be created',()=>{
    expect(service).toBeTruthy();
  });

  it('should get all paths', async()=>{
    const response ={
    "paths":[{
      "id": '1',
      "pathID":'teste1',
      "startWHId" :'ts1',
      "destinationWHId": 'ts2',
      "pathDistance" :1,
      "pathTravelTime": 1,
      "wastedEnergy": 1,
      "extraTravelTime": 1,
      }
    ],
    json(){
      return this;
    }
    };

    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));
    const warehouses ={
      startWHId: '',
      destinationWHId: ''
    }
    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });

  it('should get all destiantion paths', async()=>{
    const response ={
    "paths":[{
      "id": '1',
      "pathID":'teste1',
      "startWHId" :'ts1',
      "destinationWHId": 'ts2',
      "pathDistance" :1,
      "pathTravelTime": 1,
      "wastedEnergy": 1,
      "extraTravelTime": 1,
      }
    ],
    json(){
      return this;
    }
    };

    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));
    const warehouses ={
      startWHId: 't1',
      destinationWHId: ''
    }
    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });

  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null,"cookie");
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null","cookie");
    expect(status.status).toEqual(404);
  });
})
