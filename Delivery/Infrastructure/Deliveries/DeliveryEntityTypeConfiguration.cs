using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using EletricGo.Domain.Deliveries;
    

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
            builder.OwnsOne(b => b.destination).Property(b => b.destination).HasColumnName("destination");
            builder.OwnsOne(b => b.deliveryMass).Property(b => b.mass).HasColumnName("deliveryMass");
            //builder.Property<bool>("_active").HasColumnName("Active");
        }
    }
}