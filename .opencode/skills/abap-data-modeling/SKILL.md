---
name: abap-data-modeling
description: Modelado de datos con CDS views, asociaciones, expresiones y tipos de entidades en ABAP Cloud
license: MIT
compatibility: opencode
metadata:
  category: modeling
  focus: cds-views
  level: advanced
---

# Skill: ABAP Cloud Data Modeling

## Overview
Conceptos y mejores prácticas para modelar datos usando CDS (Core Data Services) views en ABAP Cloud.

## Key Concepts

### 1. CDS View Hierarchy

**Tipos de CDS Views**:

```
Database Tables
    ↓
Interface CDS Views (DDL Source)
    ├─ Root view entities
    ├─ View entities (child)
    └─ Association views
        ↓
Consumption CDS Views (DDL Source)
    ├─ Projection view entities
    ├─ View entities with annotations
    └─ Metadata extensions
        ↓
Service Definition
    ↓
Service Binding (OData)
```

**Interface CDS Views**:
- **DDL Source**: Define la estructura de datos
- **Root View Entities**: Entidades raíz del business object
- **View Entities**: Entidades hijas o asociadas
- **Access Control**: Reglas de autorización
- **Semantics**: Anotaciones de negocio

**Consumption CDS Views**:
- **Projection Views**: Proyecciones de interfaces
- **Metadata Extensions**: Anotaciones adicionales para UI
- **Annotations**: UI, VDM, ObjectModel
- **Value Helps**: Búsqueda de valores

### 2. CDS View Entity Types

**Root View Entity**:
```abap
@EndUserText.label: 'Root View for Incident'
@AccessControl.authorizationCheck: #CHECK
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #M, dataCategory: #MIXED }
define root view entity ZI_INCIDENT_ROOT
  as select from zt_incident as Incident
{
  key incident_id as IncidentId,
      title       as Title,
      description as Description,
      status      as Status,

      // Associations
      _History,
      _Priority,
      _Status
}
```

**View Entity**:
```abap
@EndUserText.label: 'View Entity for History'
define view entity ZI_INCIDENT_HISTORY
  as select from zt_incident_history as History
{
  key history_id as HistoryId,
      incident_id as IncidentId,
      previous_status as PreviousStatus,
      new_status as NewStatus,
      text as Text,

      // Associations
      _Incident
}
```

**Consumption View**:
```abap
@EndUserText.label: 'Consumption View for Incident'
@Metadata.allowExtensions: true
@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #M, dataCategory: #MIXED }
define view entity ZC_INCIDENT
  provider contract transactional_query
  as projection on ZI_INCIDENT_ROOT
{
  key IncidentId,
      Title,
      Description,
      Status,
      StatusDescription,
      Priority,
      PriorityDescription,

      // Associations
      _History,
      _Priority,
      _Status
}
```

### 3. Associations vs Compositions

**Associations**:
- Relaciones entre entidades independientes
- Ambas entidades pueden existir solas
- Cardinalidad: 1:1, 1:N, N:M
- **Direction**: `to` (uno a uno), `to many` (uno a muchos)

**Ejemplo**:
```abap
define view entity ZI_INCIDENT_ROOT
{
  key incident_id as IncidentId,
      priority_id as PriorityId,

      // Association - Priority puede existir sin Incident
      association [0..1] to ZI_PRIORITY as _Priority
        on $projection.PriorityId = _Priority.PriorityId
}
```

**Compositions**:
- Relaciones padre-hijo donde el hijo depende del padre
- El hijo no puede existir sin el padre
- Se usan en RAP para definir estructura del BO
- **Keyword**: `composition`

**Ejemplo**:
```abap
define root view entity ZI_INCIDENT_ROOT
{
  key incident_id as IncidentId,

      // Composition - History depende de Incident
      composition [0..*] of ZI_INCIDENT_HISTORY as _History
        on $projection.IncidentId = _History.IncidentId
}
```

**Diferencias Clave**:

| Aspecto | Association | Composition |
|---------|-------------|-------------|
| Independencia | Entidades independientes | Hijo depende del padre |
| Cardinalidad | 0..1, 1..1, 0..n, 1..n | 0..n, 1..n |
| RAP Behavior | No afecta estructura del BO | Define estructura del BO |
| Lifetime | Independientes | Padre elimina hijos (cascade) |
| Use Case | Referencias a datos existentes | Estructuras jerárquicas de datos |

### 4. CDS Expressions

**Aritméticas**:
```abap
@Semantics.amount.currencyCode: 'Currency'
TotalAmount = (Quantity * UnitPrice) as TotalAmount
```

**String**:
```abap
FullName = concat( FirstName, concat( ' ', LastName ) ) as FullName
TitleUppercase = upper( Title ) as TitleUppercase
DescriptionShort = substring( Description, 1, 50 ) as DescriptionShort
```

**Date/Time**:
```abap
@Semantics.systemDateTime.createdAt: true
CreatedAt = current_timestamp as CreatedAt
CreationDate = current_date as CreationDate
DaysSinceCreation = days_between( CreationDate, current_date ) as DaysSinceCreation
```

**Case When**:
```abap
PriorityLevel =
  case
    when Priority = 'H' then 'High'
    when Priority = 'M' then 'Medium'
    when Priority = 'L' then 'Low'
    else 'Unknown'
  end as PriorityLevel
```

**Coalesce**:
```abap
TitleOrDesc = coalesce( Title, Description, 'No Description' ) as TitleOrDesc
```

### 5. Scalar Functions

**String Functions**:
```abap
// Convertir a mayúsculas
upper( Title ) as TitleUpper

// Convertir a minúsculas
lower( Title ) as TitleLower

// Longitud
length( Title ) as TitleLength

// Substring
substring( Description, 1, 50 ) as DescriptionShort

// Concatenar
concat( FirstName, ' ', LastName ) as FullName

// Reemplazar
replace( Title, 'old', 'new' ) as TitleNew

// Trim (quitar espacios)
ltrim( Title ) as TitleLeftTrim
rtrim( Title ) as TitleRightTrim
```

**Numeric Functions**:
```abap
// Redondear
round( Amount, 2 ) as AmountRounded

// Valor absoluto
abs( Quantity ) as QuantityAbs

// Máximo/Mínimo
max( PriorityScore ) as MaxPriority
min( CreationDate ) as MinDate
```

**Date Functions**:
```abap
// Fecha actual
current_date as Today

// Timestamp actual
current_timestamp as Now

// Días entre fechas
days_between( StartDate, EndDate ) as DaysBetween

// Agregar días
add_days( StartDate, 30 ) as StartDatePlus30
```

**Type Conversion**:
```abap
// String a Integer
to_int( '123' ) as IntegerValue

// Integer a String
to_varchar( 123 ) as StringValue

// String a Date
to_date( '20240101', 'YYYYMMDD' ) as DateValue
```

### 6. CDS Data Types

**Tipos Primitivos**:
```abap
// String
title      : abap.char(100)      // Caracteres fijos
description: abap.string(255)    // String variable

// Numeric
quantity   : abap.int4           // Entero
price       : abap.dec(15,2)     // Decimal
percentage : abap.fltp            // Floating point

// Date/Time
creation_date: abap.dats          // Date (YYYYMMDD)
created_at   : abap.tims          // Time (HHMMSS)
created_ts   : abap.utclong       // Timestamp

// Boolean
is_active   : abap_boolean        // Boolean

// UUID
incident_id : sysuuid_x16         // UUID (16 bytes)
```

**Tipos de Negocio**:
```abap
// Semantics
@Semantics.amount.currencyCode: 'Currency'
amount : abap.dec(15,2)

@Semantics.quantity.unitCode: 'Unit'
quantity: abap.dec(13,3)

@Semantics.user.createdBy: true
created_by: abap.char(12)

@Semantics.systemDateTime.createdAt: true
created_at: abap.utclong

@Semantics.text: true
notes: abap.string(255)
```

## Best Practices

### 1. CDS View Design Principles

**Principios**:
- **Separation of Concerns**: Interface views vs Consumption views
- **Single Responsibility**: Cada view tiene un propósito claro
- **Performance**: Use buffer options, avoid unnecessary joins
- **Semantics**: Use appropriate semantic annotations
- **Reusability**: Create reusable view components

**Good Example**:
```abap
" Interface view - definición de datos
define root view entity ZI_INCIDENT_ROOT
  as select from zt_incident
{
  key incident_id,
      title,
      description,
      status,
      priority_id
}

" Consumption view - UI ready
define view entity ZC_INCIDENT
  provider contract transactional_query
  as projection on ZI_INCIDENT_ROOT
{
  @UI.lineItem: [{ position: 10, importance: #HIGH }]
  key incident_id as IncidentId,

  @UI.lineItem: [{ position: 20, importance: #HIGH }]
  @UI.identification: [{ position: 10 }]
  @Semantics.text: true
  title as Title,

  @UI.lineItem: [{ position: 30, importance: #MEDIUM }]
  @UI.identification: [{ position: 20 }]
  @Semantics.text: true
  description as Description,

  @ObjectModel.text.element: [ 'StatusDescription' ]
  @UI.lineItem: [{ position: 40, importance: #HIGH }]
  status as Status,

  _Status.StatusDescription as StatusDescription,
  _Priority
}
```

### 2. Association Design

**Principios**:
- Use appropriate cardinality
- Use correct direction (to vs to many)
- Use composition for parent-child relationships
- Use associations for independent relationships

**Association Cardinality Guide**:

| Caso | Cardinalidad | Example |
|-------|-------------|---------|
| Uno a uno | [0..1] to | Incident → User (one creator) |
| Uno a muchos | [0..1] to many | Incident → History (multiple entries) |
| Muchos a uno | [0..n] to | History → Incident (one parent) |
| Muchos a muchos | [0..n] to many | Projects ↔ Teams (many-to-many) |

**Composition vs Association Decision Tree**:
```
¿El hijo puede existir sin el padre?
  ├─ SÍ → Association
  └─ NO → ↓
¿El padre debe eliminar el hijo cuando se elimina?
  ├─ SÍ → Composition
  └─ NO → Association con lógica custom
```

### 3. Performance Optimization

**Buffer Options**:
```abap
@AbapCatalog.buffering.status: #ACTIVE
@AbapCatalog.buffering.type: #SINGLE
define view entity ZI_LOOKUPS
```

**Buffering Types**:
- `#SINGLE`: Single-record buffering
- `#FULL`: Full-table buffering
- `#GENERIC`: Generic-area buffering
- `#INACTIVE`: No buffering (default)

**Performance Tips**:
1. **Use Buffering**: For lookup tables, master data
2. **Avoid Deep Joins**: Don't join too many levels deep
3. **Select Only Needed Fields**: Don't SELECT *
4. **Use Indexes**: Ensure database indexes on joined fields
5. **Use Annotations**: @PerformanceHint, @QueryMonitoring

**Example**:
```abap
" GOOD: Buffered lookup table
@AbapCatalog.buffering.status: #ACTIVE
@AbapCatalog.buffering.type: #FULL
define view entity ZI_STATUS_LOOKUPS
{
  key status_code as StatusCode,
      status_description as StatusDescription
}

" BAD: Unbuffered lookup table
define view entity ZI_STATUS_LOOKUPS
{
  key status_code as StatusCode,
      status_description as StatusDescription
}
```

### 4. Semantic Annotations

**Common Semantics**:
```abap
" Money
@Semantics.amount.currencyCode: 'Currency'
amount as Amount

" Quantity
@Semantics.quantity.unitCode: 'Unit'
quantity as Quantity

" Date/Time
@Semantics.systemDateTime.createdAt: true
created_at as CreatedAt

@Semantics.user.createdBy: true
created_by as CreatedBy

" Text
@Semantics.text: true
description as Description

" ID
@Semantics.id: true
incident_id as IncidentId

" Email
@Semantics.email: true
email_address as EmailAddress

" Phone
@Semantics.phoneNumber: true
phone_number as PhoneNumber
```

**Why Use Semantics**:
- Automatic formatting in UI
- Consistent behavior across applications
- Built-in validation
- Accessibility improvements

## Common Patterns

### Pattern 1: Root View with Composition

**Scenario**: Incident Management with History

```abap
" Root View
@EndUserText.label: 'Incident Root View'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZI_INCIDENT_ROOT
  as select from zt_incident
{
  key incident_id as IncidentId,
      title as Title,
      description as Description,
      status as Status,
      priority_id as PriorityId,
      created_at as CreatedAt,
      changed_at as ChangedAt,

      // Composition - History es parte de Incident
      composition [0..*] of ZI_INCIDENT_HISTORY as _History
        on $projection.IncidentId = _History.IncidentId,

      // Association - Priority es independiente
      association [0..1] to ZI_PRIORITY as _Priority
        on $projection.PriorityId = _Priority.PriorityId
}

" Child View (Composition)
define view entity ZI_INCIDENT_HISTORY
{
  key history_id as HistoryId,
      incident_id as IncidentId,
      previous_status as PreviousStatus,
      new_status as NewStatus,
      text as Text,
      created_at as CreatedAt,

      // Association back to parent
      association [0..1] to ZI_INCIDENT_ROOT as _Incident
        on $projection.IncidentId = _Incident.IncidentId
}
```

### Pattern 2: Consumption View with UI Annotations

**Scenario**: Fiori Elements-ready view

```abap
@EndUserText.label: 'Incident Consumption View'
@Metadata.allowExtensions: true
@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #M, dataCategory: #MIXED }
define view entity ZC_INCIDENT
  provider contract transactional_query
  as projection on ZI_INCIDENT_ROOT
{
  @UI.lineItem: [{ position: 10, importance: #HIGH }]
  @UI.identification: [{ position: 10 }]
  key IncidentId,

  @UI.lineItem: [{ position: 20, importance: #HIGH }]
  @UI.identification: [{ position: 20, label: 'Título' }]
  @Semantics.text: true
  Title,

  @UI.lineItem: [{ position: 30, importance: #MEDIUM }]
  @UI.identification: [{ position: 30 }]
  @Semantics.text: true
  @UI.multiLineText: true
  Description,

  @ObjectModel.text.element: [ 'StatusDescription' ]
  @UI.lineItem: [{ position: 40, importance: #HIGH }]
  @UI.identification: [{ position: 40, label: 'Estado' }]
  Status,
  _Status.StatusDescription as StatusDescription,

  @ObjectModel.text.element: [ 'PriorityDescription' ]
  @UI.lineItem: [{ position: 50, importance: #MEDIUM }]
  Priority,
  _Priority.PriorityDescription as PriorityDescription,

  @UI.lineItem: [{ position: 60, importance: #LOW }]
  @Semantics.systemDateTime.createdAt: true
  CreatedAt,

  // Associations
  _History,
  _Priority,
  _Status
}
```

### Pattern 3: Value Help View

**Scenario**: Lookup table for Status codes

```abap
" Value Help View
@EndUserText.label: 'Status Value Help'
@Search.searchable: true
define view entity ZI_STATUS_VH
  as select from zt_status
{
  key status_code as StatusCode,
      status_description as StatusDescription
}

" Usage in Consumption View
define view entity ZC_INCIDENT
{
  @Consumption.valueHelpDefinition: [{
    entity.name: 'ZI_STATUS_VH',
    entity.element: 'StatusCode',
    useForValidation: true
  }]
  Status,

  _Status.StatusDescription as StatusDescription
}
```

## Common Pitfalls

### Pitfall 1: Mixing Associations and Compositions

**Error**: Using composition for independent entities

```abap
" WRONG: Priority shouldn't be a composition
composition [0..1] of ZI_PRIORITY as _Priority
```

**Consequence**: Priority would be deleted when Incident is deleted

**Solution**: Use association instead
```abap
" CORRECT: Priority is independent
association [0..1] to ZI_PRIORITY as _Priority
```

### Pitfall 2: Missing Keys in Joins

**Error**: Joining on non-key fields

```abap
" WRONG: Join on description (not key)
association to ZI_STATUS as _Status
  on $projection.Status = _Status.StatusDescription  // ERROR
```

**Consequence**: Duplicate rows, incorrect results, performance issues

**Solution**: Join on key fields
```abap
" CORRECT: Join on key field (status_code)
association to ZI_STATUS as _Status
  on $projection.Status = _Status.StatusCode
```

### Pitfall 3: Ignoring Semantics

**Error**: Not using semantic annotations

```abap
" WRONG: No semantics
define view entity ZC_INCIDENT
{
  amount as Amount,
  quantity as Quantity,
  created_at as CreatedAt
}
```

**Consequence**: UI doesn't format correctly, no automatic validation

**Solution**: Use semantic annotations
```abap
" CORRECT: With semantics
define view entity ZC_INCIDENT
{
  @Semantics.amount.currencyCode: 'Currency'
  amount as Amount,

  @Semantics.quantity.unitCode: 'Unit'
  quantity as Quantity,

  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt
}
```

### Pitfall 4: Overfetching Data

**Error**: Selecting too many fields unnecessarily

```abap
" WRONG: Selecting all fields
SELECT * FROM zt_incident WHERE incident_id = '123'
```

**Consequence**: Performance issues, network overhead

**Solution**: Select only needed fields
```abap
" CORRECT: Selecting only needed fields
SELECT incident_id, title, status
  FROM zt_incident
 WHERE incident_id = '123'
```

## Troubleshooting

### Issue: Association Not Working

**Symptoms**: Association data not available, null values

**Possible Causes**:
1. Wrong join condition
2. Missing data in joined table
3. Wrong cardinality
4. Access control issues

**Solutions**:
1. Verify join condition matches keys
2. Check data exists in joined table
3. Verify cardinality is correct
4. Check access control in both views

### Issue: Performance Issues

**Symptoms**: Slow queries, timeouts

**Possible Causes**:
1. Missing database indexes
2. Deep joins (too many levels)
3. No buffering on lookup tables
4. Selecting too many fields

**Solutions**:
1. Create indexes on joined fields
2. Limit join depth (max 3-4 levels)
3. Add buffering to lookup tables
4. Select only necessary fields

### Issue: Semantics Not Working

**Symptoms**: UI not formatting correctly, validation not working

**Possible Causes**:
1. Wrong annotation syntax
2. Missing annotations
3. Wrong semantic type
4. Consumption view not updated

**Solutions**:
1. Verify annotation syntax
2. Add all necessary annotations
3. Use correct semantic type
4. Regenerate consumption view

## References

### Official SAP Documentation
- SAP Help Portal - CDS: https://help.sap.com/doc/abapdocu_latest_index_enus/index.htm
- Core Data Services (CDS) Guide
- CDS View Entity Syntax
- Semantic Annotations

### Tools
- ADT (ABAP Development Tools) for Eclipse
- Data Preview in ADT
- SQL Trace (ST05)
- Performance Analyzer (SAT)

### Best Practices
- SAP CDS Modeling Guidelines
- Performance Tuning for CDS
- Semantic Annotation Catalog

---

*This skill provides comprehensive guidance for data modeling with CDS views in ABAP Cloud. Always refer to official SAP documentation for the most current information.*
