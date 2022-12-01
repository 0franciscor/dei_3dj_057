import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDialogModule } from '@angular/material/dialog';
import { RouterModule } from '@angular/router';

import { EditWarehouseComponent,EditWarehouseComponentDialog } from './edit-warehouse.component';

describe('EditWarehouseComponent', () => {
  let component: EditWarehouseComponent;
  let fixture: ComponentFixture<EditWarehouseComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditWarehouseComponent, EditWarehouseComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, RouterModule.forRoot([]),],
      
    })
    .compileComponents();

    fixture = TestBed.createComponent(EditWarehouseComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
