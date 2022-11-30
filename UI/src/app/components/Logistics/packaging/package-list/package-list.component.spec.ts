import { ComponentFixture,TestBed } from "@angular/core/testing";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatDialogModule } from "@angular/material/dialog";
import { PackageListComponent } from "./package-list.component";

describe('PackageListComponent', () => {
    let component: PackageListComponent;
    let fixture: ComponentFixture<PackageListComponent>;

    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [PackageListComponent ],
            imports: [MatDialogModule,FormsModule,ReactiveFormsModule]
        }).compileComponents();

        fixture = TestBed.createComponent(PackageListComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});

