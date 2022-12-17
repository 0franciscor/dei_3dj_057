import { ComponentFixture,TestBed } from "@angular/core/testing";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatCardModule } from "@angular/material/card";
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from "@angular/material/dialog";
import { MatFormFieldModule } from "@angular/material/form-field";
import { MatInputModule } from "@angular/material/input";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { TruckService } from "src/app/Services/TruckService/truck.service";
import { CreatePackageComponent } from "./create-package.component";


describe('CreatePackageComponent', () => {
    let component: CreatePackageComponent;
    let fixture: ComponentFixture<CreatePackageComponent>;
    const dialogMock = {
        close: () => { }
        };
    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [CreatePackageComponent ],
            imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
            providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
        }).compileComponents();

        fixture = TestBed.createComponent(CreatePackageComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});