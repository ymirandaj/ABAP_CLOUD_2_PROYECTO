---
name: abap-behavior-definition
description: BDL (Behavior Definition Language) para definir comportamiento de business objects RAP
license: MIT
compatibility: opencode
metadata:
  category: behavior
  focus: bdl-syntax
  level: advanced
---

# Skill: ABAP Behavior Definition (BDL)

## Overview
Comprehensive guide to Behavior Definition Language (BDL) for defining behavior of RAP business objects in ABAP Cloud.

## Key Concepts

### 1. BDL Structure

**Basic Structure**:
```abap
managed implementation in class zbp_<bo_name> unique;
strict ( 2 );
with draft;

define behavior for <interface_cds> alias <alias>

persistent table <db_table>
draft table <draft_table>

etag master <field>
lock master total etag <field>
authorization master ( global, instance )

{
  // Field properties
  field ( properties ) <fields>;

  // CRUD operations
  create;
  update;
  delete;

  // Associations
  association <assoc> { create; with draft; };

  // Actions
  action ( features ) <action_name> result [1] $self;

  // Determinations
  determination <determination_name> on event { fields; };

  // Validations
  validation <validation_name> on event { fields; };

  // Side effects
  side effects
  {
    action <action_name> affects entity <entity>;
  }

  // Mapping
  mapping for <db_table> corresponding
  {
    <cds_field> = <db_field>;
    <cds_field> = <db_field>;
  }
}
```

### 2. Implementation Types

**Managed**:
```abap
managed implementation in class zbp_incident unique;
```

**Characteristics**:
- Framework generates most CRUD operations automatically
- Only custom logic needs to be implemented
- Draft enabled by default
- Optimized for standard use cases

**When to Use**:
- Standard CRUD operations
- Most business objects
- When framework-generated behavior is sufficient

**Unmanaged**:
```abap
define behavior for ZI_INCIDENT alias Incident
```

**Characteristics**:
- Full control over behavior
- No automatic CRUD generation
- All operations must be implemented manually
- More complex but flexible

**When to Use**:
- Complex custom logic
- Non-standard operations
- When full control is required

### 3. Draft vs Non-Draft

**With Draft**:
```abap
with draft;
```

**Characteristics**:
- Supports draft editing
- User can edit without saving immediately
- Better UX for complex forms
- Requires additional tables

**Usage**:
```abap
managed implementation in class zbp_incident unique;
strict ( 2 );
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft
```

**Draft Actions** (automatic):
- `Activate`: Promote draft to active
- `Discard`: Delete draft
- `Edit`: Start editing (create draft)
- `Resume`: Resume editing existing draft
- `Prepare`: Initialize draft

**Without Draft**:
```abap
" No 'with draft' keyword
```

**Characteristics**:
- Immediate save
- Simpler implementation
- Less storage overhead
- Worse UX for complex editing

### 4. Field Properties

**Readonly**:
```abap
field ( readonly ) IncidentId, CreatedAt, CreatedBy;
```

**Mandatory**:
```abap
field ( mandatory ) Title, Description, Priority;
```

**Readonly with Numbering**:
```abap
field ( numbering : managed, readonly ) IncUUID;
```

**Features (Instance)**:
```abap
field ( features : instance ) CreationDate, ChangedDate;
```

**All Properties**:
```abap
// Readonly
field ( readonly ) <fields>;

// Mandatory
field ( mandatory ) <fields>;

// Readonly + Numbering
field ( numbering : managed, readonly ) <field>;

// Instance features
field ( features : instance ) <fields>;

// Multiple properties
field ( readonly, mandatory ) <fields>;
```

**Property Combinations**:

| Property | Combinations | Example |
|----------|--------------|---------|
| readonly | alone | `field ( readonly ) IncidentId` |
| readonly | + numbering | `field ( numbering : managed, readonly ) IncUUID` |
| mandatory | alone | `field ( mandatory ) Title` |
| mandatory | + readonly | ❌ Not allowed |
| features | alone | `field ( features : instance ) CreationDate` |
| features | + readonly | `field ( readonly, features : instance ) LastChangedAt` |

### 5. CRUD Operations

**Create**:
```abap
create;
```

**Update**:
```abap
update;
```

**Delete**:
```abap
delete;
```

**All CRUD**:
```abap
create;
update;
delete;
```

**Standard CRUD (Managed)**:
```abap
managed implementation in class zbp_incident unique;
strict ( 2 );
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

{
  create;  // Framework generates create logic
  update;  // Framework generates update logic
  delete;  // Framework generates delete logic
}
```

**Custom CRUD (Unmanaged)**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  create;  // Must implement create logic
  update;  // Must implement update logic
  delete;  // Must implement delete logic
}
```

### 6. Associations

**Basic Association**:
```abap
association _History { create; with draft; };
```

**Association without Create**:
```abap
association _Priority;
```

**Association with Draft**:
```abap
association _History { create; with draft; };
```

**Association Types**:

| Type | Syntax | Meaning |
|------|--------|---------|
| Read-only | `association _Assoc;` | Can only read association |
| Create | `association _Assoc { create; }` | Can create child entries |
| Create + Draft | `association _Assoc { create; with draft; }` | Can create with draft support |
| Update | `association _Assoc { update; }` | Can update child entries |
| Delete | `association _Assoc { delete; }` | Can delete child entries |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  // Association to history - can create with draft
  association _History { create; with draft; };

  // Association to priority - read-only
  association _Priority;

  // Association to status - can read and update
  association _Status { update; };
}
```

### 7. Actions

**Basic Action**:
```abap
action ( features : instance ) changeStatus
  result [1] $self;
```

**Action with Parameters**:
```abap
action ( features : instance ) changeStatus
  parameter ZDD_AE_CHANGE_STATUS
  result [1] $self;
```

**Action Features**:
```abap
action ( features : instance, authorization : update ) approve
  result [1] $self;
```

**Action Result Types**:

| Result | Meaning | Use Case |
|--------|---------|----------|
| `result [1] $self` | Returns modified instance | Most common |
| `result [1..*] $self` | Returns multiple instances | Rare |
| `result [1] $self\_Assoc` | Returns with association | When association data needed |

**Example with All Features**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  // Basic action
  action ( features : instance ) changeStatus
    result [1] $self;

  // Action with parameters
  action ( features : instance, authorization : update ) approve
    parameter ZDD_AE_APPROVE_PARAM
    result [1] $self;

  // Static action (no instance required)
  action ( authorization : create ) createIncident
    parameter ZDD_AE_CREATE_PARAM
    result [1] $self;
}
```

**Action Parameters**:
```abap
" Parameter CDS view
@EndUserText.label: 'Change Status Parameters'
define abstract entity ZDD_AE_CHANGE_STATUS
{
  @Consumption.valueHelpDefinition: [{
    entity.name: 'ZI_STATUS_VH',
    entity.element: 'StatusCode',
    useForValidation: true
  }]
  status : zde_status;

  @EndUserText.label: 'Observation Text'
  @UI.multiLineText: true
  text : zde_text;
}
```

### 8. Determinations

**Basic Determination**:
```abap
determination setStatus on modify { Status; };
```

**Determination on Create**:
```abap
determination setInitialValues on modify { create; };
```

**Determination on Save**:
```abap
determination createInitialHistory on save { create; };
```

**Determination on Multiple Fields**:
```abap
determination calculateFields on modify { Status, Priority, Quantity; };
```

**Determination Types**:

| Trigger | Event | When Fired |
|---------|-------|------------|
| `on modify` | Any modification | When fields are modified |
| `on save` | Save operation | When data is saved |
| `on activate` | Draft activate | When draft is activated |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  // Determination when Incident is created
  determination setInitialValues on modify { create; };

  // Determination when Status is modified
  determination setStatus on modify { Status; };

  // Determination when data is saved
  determination createInitialHistory on save { create; };

  // Determination on multiple fields
  determination calculateFields on modify { Status, Priority, Quantity; };
}
```

### 9. Validations

**Basic Validation**:
```abap
validation validateStatus on save { Status; };
```

**Validation on Create**:
```abap
validation validateMandatory on save { create; Title, Description; };
```

**Validation on Update**:
```abap
validation validateStatusChange on save { update; Status; };
```

**Validation on Multiple Events**:
```abap
validation validateData on save { create; update; Title, Status; };
```

**Validation Types**:

| Event | Meaning | When Fired |
|-------|---------|------------|
| `create` | Create operation | When creating new data |
| `update` | Update operation | When updating existing data |
| `delete` | Delete operation | When deleting data |
| `create; update` | Create or update | When creating or updating |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  // Validation when creating
  validation validateMandatory on save { create; Title, Description; };

  // Validation when updating status
  validation validateStatusChange on save { update; Status; };

  // Validation on create or update
  validation validateData on save { create; update; Title, Status; };
}
```

### 10. Side Effects

**Basic Side Effect**:
```abap
side effects
{
  action changeStatus affects entity _History;
}
```

**Side Effect for Field**:
```abap
side effects
{
  field Status affects entity _History;
}
```

**Multiple Side Effects**:
```abap
side effects
{
  action changeStatus affects entity _History;
  field Status affects entity _History;
  action changeStatus affects entity Incident;
}
```

**Side Effect Types**:

| Type | Trigger | Affected Entity |
|------|---------|-----------------|
| `action <name> affects entity <entity>` | Action executed | Entity to reload |
| `field <name> affects entity <entity>` | Field modified | Entity to reload |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

{
  action ( features : instance ) changeStatus
    result [1] $self;

  association _History { create; with draft; };

  side effects
  {
    // When action changeStatus is executed, reload _History
    action changeStatus affects entity _History;

    // When field Status is modified, reload _History
    field Status affects entity _History;
  }
}
```

**Important Notes**:
- Side effects tell the UI what to reload after an action/field change
- Essential for automatic refresh of compositions
- Works with Fiori Elements automatically
- Does NOT implement logic - only declarative

### 11. Mapping

**Basic Mapping**:
```abap
mapping for zt_incident corresponding
{
  IncidentID  = incident_id;
  Title       = title;
  Description = description;
  Status      = status;
}
```

**Complete Mapping**:
```abap
define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

{
  // Field properties, operations, etc.

  mapping for zt_incident corresponding
  {
    IncidentID       = incident_id;
    Title            = title;
    Description      = description;
    Status           = status;
    PriorityID       = priority_id;
    CreationDate     = creation_date;
    ChangedDate      = changed_date;
    CreatedBy        = local_created_by;
    CreatedAt        = local_created_at;
    ChangedBy        = local_last_changed_by;
    ChangedAt        = local_last_changed_at;
    LastChangedAt    = last_changed_at;
  }
}
```

**Mapping Rules**:
- CDS field name = Database field name
- Case insensitive (but maintain consistency)
- All CDS fields must be mapped
- Draft table mapping is automatic

### 12. ETag and Lock

**ETag Master**:
```abap
etag master LocalLastChangedAt;
```

**Lock Master Total ETag**:
```abap
lock master total etag LastChangedAt;
```

**Both ETag and Lock**:
```abap
etag master LocalLastChangedAt;
lock master total etag LastChangedAt;
```

**ETag Types**:

| Type | Meaning | Use Case |
|------|---------|----------|
| `etag master <field>` | Version control field | Optimistic locking |
| `lock master total etag <field>` | Lock entire entity | Pessimistic locking |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

etag master LocalLastChangedAt
lock master total etag LastChangedAt

{
  // ETag provides optimistic locking
  // Lock provides pessimistic locking
}
```

**Fields for ETag/Lock**:
- Should be a timestamp field
- Updated on every modify
- Example: `last_changed_at`, `local_last_changed_at`

### 13. Authorization

**Global Authorization**:
```abap
authorization master ( global );
```

**Instance Authorization**:
```abap
authorization master ( global, instance );
```

**Authorization Types**:

| Type | Meaning | Implementation |
|------|---------|----------------|
| `global` | Global authorization check | Can user access this BO at all? |
| `instance` | Instance authorization check | Can user access this specific instance? |

**Example**:
```abap
define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

authorization master ( global, instance )

{
  // Implement get_global_authorizations and get_instance_authorizations
  // in the behavior implementation class
}
```

### 14. Strict Mode

**Strict Level 1**:
```abap
strict ( 1 );
```

**Strict Level 2** (recommended):
```abap
strict ( 2 );
```

**Strict Mode Benefits**:
- Stricter syntax validation
- Better performance
- More checks at compile time
- Prevents anti-patterns

**Recommendation**: Always use `strict ( 2 )`

## Best Practices

### 1. Behavior Definition Structure

**Good Structure**:
```abap
" 1. Implementation type
managed implementation in class zbp_incident unique;

" 2. Strict mode
strict ( 2 );

" 3. Draft support
with draft;

" 4. Define behavior
define behavior for ZI_INCIDENT alias Incident

" 5. Persistent and draft tables
persistent table zt_incident
draft table zt_incident_draft

" 6. ETag and Lock
etag master LocalLastChangedAt
lock master total etag LastChangedAt

" 7. Authorization
authorization master ( global, instance )

{
  " 8. Field properties
  field ( numbering : managed, readonly ) IncidentID;
  field ( mandatory ) Title;
  field ( readonly ) CreatedAt;

  " 9. CRUD operations
  create;
  update;
  delete;

  " 10. Associations
  association _History { create; with draft; };

  " 11. Actions
  action ( features : instance ) changeStatus result [1] $self;

  " 12. Determinations
  determination setInitialValues on modify { create; };
  determination setStatus on modify { Status; };

  " 13. Validations
  validation validateStatus on save { Status; };

  " 14. Side effects
  side effects
  {
    action changeStatus affects entity _History;
    field Status affects entity _History;
  }

  " 15. Mapping
  mapping for zt_incident corresponding
  {
    IncidentID = incident_id;
    Title = title;
    Status = status;
  }
}
```

### 2. Field Property Best Practices

**Readonly Fields**:
```abap
" Readonly fields should be:
" - System fields (ID, timestamps, user fields)
" - Calculated fields (not user-modifiable)
field ( readonly ) IncidentID, CreatedAt, CreatedBy, ChangedAt, ChangedBy;
```

**Mandatory Fields**:
```abap
" Mandatory fields should be:
" - Business-critical data
" - Required for business logic
field ( mandatory ) Title, Description, Priority;
```

**Numbered Fields**:
```abap
" Numbered fields should be:
" - Primary keys (UUIDs)
" - Sequential IDs
field ( numbering : managed, readonly ) IncidentID;
```

### 3. Action Best Practices

**Action Naming**:
```abap
" GOOD: Verb-based, clear purpose
action ( features : instance ) changeStatus result [1] $self;
action ( features : instance ) approveIncident result [1] $self;
action ( features : instance ) rejectIncident result [1] $self;

" BAD: Vague, unclear purpose
action ( features : instance ) doSomething result [1] $self;
action ( features : instance ) process result [1] $self;
```

**Action Features**:
```abap
" Use appropriate features
action ( features : instance, authorization : update ) changeStatus result [1] $self;

// features: instance = Action applies to a specific instance
// authorization: update = User needs update authorization
```

**Action Result**:
```abap
" Most actions should return $self
action changeStatus result [1] $self;

" Only use $self\_Assoc if you need association data
action changeStatusWithHistory result [1] $self\_History;
```

### 4. Determination Best Practices

**Determination Naming**:
```abap
" GOOD: Descriptive, indicates what it does
determination setInitialValues on modify { create; };
determination calculateStatus on modify { Priority, Urgency; };
determination updateTimestamp on modify { Status; };

" BAD: Vague, unclear purpose
determination determination1 on modify { create; };
determination doSomething on modify { Status; };
```

**Determination Events**:
```abap
" Use appropriate events
determination setInitialValues on modify { create; };    // Only on create
determination updateTimestamp on modify { Status; };      // When field changes
determination calculateFields on save { create; };       // Before saving
```

### 5. Validation Best Practices

**Validation Naming**:
```abap
" GOOD: Descriptive, indicates what it validates
validation validateMandatory on save { create; Title, Description; };
validation validateStatusTransition on save { update; Status; };
validation validateQuantity on save { create; update; Quantity; };

" BAD: Vague, unclear purpose
validation check1 on save { Status; };
validation doCheck on save { Title; };
```

**Validation Scope**:
```abap
" Validate only what's necessary
validation validateMandatory on save { create; Title, Description; };  // OK

" Don't validate unrelated fields
validation validateAllFields on save { Title, Status, Priority, CreatedAt }; // BAD
```

### 6. Side Effect Best Practices

**When to Use Side Effects**:
```abap
" Use side effects when:
" - You have compositions (parent-child)
" - The UI needs to reload child data after an action
" - You want automatic refresh without page reload

side effects
{
  action changeStatus affects entity _History;  // GOOD
  field Status affects entity _History;         // GOOD
}
```

**When NOT to Use Side Effects**:
```abap
" Don't use side effects for:
" - Unrelated entities
" - Independent associations (not compositions)

side effects
{
  action changeStatus affects entity _Priority;  // BAD: Not a composition
  field Status affects entity _Status;          // BAD: Not a composition
}
```

## Common Patterns

### Pattern 1: Standard CRUD BO with Draft

```abap
managed implementation in class zbp_incident unique;
strict ( 2 );
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

etag master LocalLastChangedAt
lock master total etag LastChangedAt
authorization master ( global, instance )

{
  field ( numbering : managed, readonly ) IncidentID;
  field ( mandatory ) Title, Description;
  field ( readonly ) CreatedAt, CreatedBy, ChangedAt, ChangedBy;

  create;
  update;
  delete;

  association _History { create; with draft; };

  determination setInitialValues on modify { create; };
}
```

### Pattern 2: BO with Actions

```abap
managed implementation in class zbp_incident unique;
strict ( 2 );
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

{
  field ( numbering : managed, readonly ) IncidentID;
  field ( mandatory ) Title;

  create;
  update;
  delete;

  action ( features : instance ) changeStatus
    parameter ZDD_AE_CHANGE_STATUS
    result [1] $self;

  action ( features : instance, authorization : update ) approve
    result [1] $self;

  action ( features : instance, authorization : update ) reject
    result [1] $self;
}
```

### Pattern 3: BO with Compositions and Side Effects

```abap
managed implementation in class zbp_incident unique;
strict ( 2 );
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft

{
  create;
  update;
  delete;

  association _History { create; with draft; };

  action ( features : instance ) changeStatus
    result [1] $self;

  side effects
  {
    action changeStatus affects entity _History;
    field Status affects entity _History;
  }
}
```

## Common Pitfalls

### Pitfall 1: Wrong Field Mapping

**Error**:
```abap
mapping for zt_incident corresponding
{
  IncidentID  = incident_id;      // OK
  Title       = description;      // WRONG: Mismatch
  Description = title;            // WRONG: Mismatch
}
```

**Consequence**: Data corruption, wrong values saved

**Solution**:
```abap
mapping for zt_incident corresponding
{
  IncidentID  = incident_id;      // OK
  Title       = title;            // OK
  Description = description;      // OK
}
```

### Pitfall 2: Missing Draft Table

**Error**:
```abap
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
" ERROR: No draft table!

{
  create;
  update;
  delete;
}
```

**Consequence**: Activation error, draft not working

**Solution**:
```abap
with draft;

define behavior for ZI_INCIDENT alias Incident

persistent table zt_incident
draft table zt_incident_draft  // OK: Draft table defined
{
  create;
  update;
  delete;
}
```

### Pitfall 3: Inconsistent Action Names

**Error**:
```abap
" BDEF
action changeStatus result [1] $self;

" Implementation class
METHOD change_status.  // WRONG: Inconsistent naming
```

**Consequence**: Activation error, action not found

**Solution**:
```abap
" BDEF
action changeStatus result [1] $self;

" Implementation class
METHOD changeStatus.  // OK: Consistent naming
```

### Pitfall 4: Wrong Side Effect Syntax

**Error**:
```abap
side effects
{
  action changeStatus affects entity _History , $self;  // WRONG: Comma + $self
}
```

**Consequence**: Syntax error, side effect not working

**Solution**:
```abap
side effects
{
  action changeStatus affects entity _History;  // OK: Correct syntax
}
```

## Troubleshooting

### Issue: Activation Error

**Symptoms**: Behavior definition won't activate

**Possible Causes**:
1. Missing draft table
2. Wrong field mapping
3. Syntax error in BDEF
4. Missing implementation class

**Solutions**:
1. Verify draft table exists
2. Check field mapping matches CDS and DB
3. Fix syntax errors
4. Verify implementation class exists

### Issue: Action Not Working

**Symptoms**: Action not available or not executing

**Possible Causes**:
1. Action not implemented in behavior class
2. Authorization issue
3. Features incorrectly configured
4. Wrong action name

**Solutions**:
1. Implement action method in behavior class
2. Check user authorizations
3. Verify features configuration
4. Verify action name consistency

### Issue: Determination Not Firing

**Symptoms**: Determination not executing

**Possible Causes**:
1. Wrong event configured
2. Wrong fields specified
3. Implementation class method not found
4. Not in strict mode

**Solutions**:
1. Verify event (create, modify, save)
2. Check fields list
3. Implement determination method in behavior class
4. Use `strict ( 2 )`

## References

### Official SAP Documentation
- SAP Help Portal - BDL: https://help.sap.com/doc/abapdocu_latest_index_enus/index.htm
- Behavior Definition Language Guide
- RAP Behavior Guide

### Tools
- ADT (ABAP Development Tools) for Eclipse
- Behavior Definition Editor in ADT
- Activation Monitor

### Best Practices
- SAP RAP Development Guide
- Behavior Definition Patterns
- Side Effects in RAP

---

*This skill provides comprehensive guidance for Behavior Definition Language in ABAP Cloud. Always refer to official SAP documentation for the most current information.*
