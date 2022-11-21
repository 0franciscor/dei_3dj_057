using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.ValueObjects;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;


namespace EletricGo.Infrastructure.Deliveries
{
    internal class DeliveryEntityTypeConfiguration : IEntityTypeConfiguration<Delivery>
    {
        public void Configure(EntityTypeBuilder<Delivery> builder)  
        {
            // cf. https://www.entityframeworktutorial.net/efcore/fluent-api-in-entity-framework-core.aspx
            builder.ToTable("Delivery", SchemaNames.EletricGo);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.deliveryDate).Property(b => b.date).HasColumnName("deliveryDate");
            builder.OwnsOne(b => b.loadTime).Property(b => b.time).HasColumnName("loadTime");
            builder.OwnsOne(b => b.unloadTime).Property(b => b.time).HasColumnName("unloadTime");
            builder.OwnsOne(b => b.deliveryMass).Property(b => b.mass).HasColumnName("deliveryMass");
            builder.HasOne(b => b.destinationWarehouse).WithMany().HasForeignKey(b => b.destination);
            //builder.Property(b => b.destinationId).HasConversion(v => v, v => new WarehouseId(v));

            /*builder.HasOne(b => b.warehouse).WithMany().HasForeignKey(b => b.warehouseId);
            builder.Property(b => b.warehouseId).HasConversion(v => v.value, v => new WarehouseId(v));*/
        }
    }
}