import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { PackagingService } from 'src/app/Services/PackageService/package.service';

import { PackageListComponent } from './package-list.component';

describe('PackageListComponent', () => {
  let component: PackageListComponent;
  let fixture: ComponentFixture<PackageListComponent>;
  let fakePackagingService: any;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [PackageListComponent],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule],
      providers: [PackagingService, { provide: MatDialogRef, useValue: {} }, { provide: MAT_DIALOG_DATA, useValue: {} }]

    })
      .compileComponents();

      fakePackagingService = jasmine.createSpyObj('PackagingService', ['getPackage']);
      fakePackagingService.getPackage.and.returnValue(Promise.resolve({ status: 200 }));

    TestBed.overrideProvider(PackagingService, { useValue: fakePackagingService });
    fixture = TestBed.createComponent(PackageListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

describe('fakePackagingService', () => {
  let service: PackagingService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PackagingService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should get all packages', async () => {
    const response = {
      "packages": [
        {  
        "packagingID": "geg",
        "truckID": "newTruck",
        "deliveryID": "4438",
        "xPosition": 4 ,
        "yPosition": 3,
        "zPosition": 2
        }
      ],
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const packages = await service.getPackage();
    expect(fetchSpy).toHaveBeenCalled();
    expect(packages).toEqual(response);
  });

});