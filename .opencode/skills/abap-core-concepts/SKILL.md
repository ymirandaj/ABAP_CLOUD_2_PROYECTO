---
name: abap-core-concepts
description: Conceptos fundamentales de ABAP Cloud, Clean Core y extensibilidad SAP
license: MIT
compatibility: opencode
metadata:
  category: fundamentals
  focus: clean-core
  level: advanced
---

# Skill: ABAP Cloud Core Concepts

## Overview
Conceptos fundamentales para desarrollar en ABAP Cloud respetando los principios de Clean Core de SAP.

## Key Concepts

### 1. Clean Core

**Definición**: Principio de mantener el núcleo estándar de SAP limpio y libre de modificaciones directas.

**Principios**:
- **No modifications to SAP standard code**: Nunca modificar código estándar directamente
- **Use extension points only**: Usar únicamente mecanismos de extensión provistos
- **Leverage released APIs**: Utilizar solo APIs released por SAP
- **Follow SAP development guidelines**: Seguir guías de desarrollo SAP
- **Maintain upgradeability**: Mantener capacidad de actualización

**Benefits**:
- Fácil actualización a nuevas versiones
- Menos bugs y problemas de compatibilidad
- Soporte oficial de SAP garantizado
- Mejor mantenibilidad del código

### 2. Extension Points

**Jerarquía de Extensiones**:
```
SAP Standard (Core)
├── Released APIs (C1/C2)
│   ├── APIs estables (C1)
│   ├── APIs deprecated pero soportadas (C2)
│   └── APIs solo para desarrollo (D)
├── Business Add-ins (BAdIs)
│   ├── Single use
│   └── Multiple use
├── Custom Fields and Logic (CDS Views)
│   ├── View Extensions
│   └── Metadata Extensions
├── Custom Business Objects
│   ├── RAP Services
│   └── Custom Tables
└── Side-by-Side Extensions (BTP)
    ├── Cloud Foundry
    └── CAP Applications
```

**Tipos de Extension Points**:

| Tipo | Caso de Uso | Prioridad |
|------|-------------|-----------|
| Released APIs | Integración con funcionalidad estándar | Alta |
| BAdIs | Extend lógica de negocio existente | Alta |
| CDS Extensions | Enricher datos de vistas estándar | Media |
| Custom BOs | Crear nuevas entidades de negocio | Media |
| Side-by-Side | Extensiones complejas fuera del core | Baja |

### 3. ABAP Cloud Development Model

**Tier 1 - Cloud Ready**:
- Desarrollado siguiendo principios Cloud
- Compatible con ABAP Environment
- Usa released APIs exclusivamente

**Tier 2 - Cloud API Enablement**:
- ABAP tradicional con APIs released
- Migración progresiva
- Preparado para Cloud

**ABAP Cloud Strict**:
- Solo código cloud-ready
- Sin deprecated APIs
- Máxima portabilidad

### 4. Extensiones vs Modifications

**Modificaciones (NO usar en Clean Core)**:
- Cambios directos a código estándar
- Modificación de BDEF/CDS estándar
- Overrides de comportamiento estándar
- Direct table access a tablas estándar

**Extensiones (CORRECTO en Clean Core)**:
- BAdI implementations
- CDS view extensions
- Metadata extensions
- Custom fields via Custom Fields and Logic app
- Released APIs
- Side-by-Side extensions

## Best Practices

### 1. Estrategia de Extensión

**Preguntas antes de extender**:
1. ¿Existe funcionalidad estándar que resuelva el requerimiento?
2. ¿Hay released APIs disponibles?
3. ¿Hay BAdIs disponibles?
4. ¿Se puede lograr con CDS extensions?
5. ¿Es necesario un custom BO o side-by-side extension?

**Flowchart de Decisión**:
```
Requerimiento de Negocio
    ↓
¿Funcionalidad estándar existe?
  ├─ SÍ → Usar estándar
  └─ NO → ↓
¿Released API disponible?
  ├─ SÍ → Usar released API
  └─ NO → ↓
¿BAdI disponible?
  ├─ SÍ → Implementar BAdI
  └─ NO → ↓
¿CDS Extension suficiente?
  ├─ SÍ → Extender vista CDS
  └─ NO → ↓
¿Necesario custom BO?
  ├─ SÍ → Crear Custom RAP BO
  └─ NO → Side-by-Side extension
```

### 2. Upgrade Safe Development

**Principios**:
- Separar código custom de estándar
- Usar nombres estandarizados (prefix Z/Y)
- Documentar todas las extensiones
- Testear en sandbox antes de producción
- Monitorear notices de deprecation

**Checklist**:
- [ ] Solo código custom en objetos Z/Y
- [ ] Solo released APIs usadas
- [ ] Extension points identificados
- [ ] Código documentado
- [ ] ATC checks pasados
- [ ] Testeados en sandbox

### 3. Released APIs

**Tipos de Release**:
- **C1**: Stable, compatible release (recomendado para producción)
- **C2**: Deprecated, still supported (migrar a C1 cuando sea posible)
- **D**: Development purpose only (no usar en producción)

**Encontrar Released APIs**:
- Transaction API_HUB en SAP S/4HANA
- SAP API Business Hub: https://api.sap.com/
- Package SAP_API_* releases
- CDS views con @API.* annotations

### 4. Naming Conventions

**Custom Objects**:
- Prefix: `Z_` o `Y_`
- Pattern: `Z_<FunctionalArea>_<Purpose>_<Type>`
- Ejemplos:
  - `Z_INCT_MNG_RAP_BO` (Incident Management RAP Business Object)
  - `Z_INCT_SRV_BND` (Incident Service Binding)
  - `Z_INCT_CDS_EXT` (Incident CDS Extension)

**Estructura de Paquetes**:
```
Z_<Area>
├── _CDS (Views CDS)
├── _DDL (DDL Sources)
├── _BDEF (Behavior Definitions)
├── _CLAS (Classes)
├── _TABL (Tables)
└── _SRV (Services)
```

## Common Patterns

### Pattern 1: In-App Extension

**Cuándo usar**: Extender objetos estándar con campos o lógica adicional

**Pasos**:
1. Usar app "Custom Fields and Logic"
2. Crear campos custom
3. Generar CDS extension
4. Opcional: Implementar BAdI para lógica

**Ejemplo**:
```abap
" CDS Extension View
@EndUserText.label: 'Extension for Standard Object'
@AbapCatalog.viewEnhancementCategory: #PROJECTION_LIST
define view entity Z_STD_OBJ_EXT
  as projection on ZI_STD_OBJECT
{
  key KeyField,
      StandardField,
      Z_CustomField,  " Campo custom creado via Custom Fields and Logic
      _CustomAssociation
}
```

### Pattern 2: BAdI Implementation

**Cuándo usar**: Extender lógica de negocio estándar

**Pasos**:
1. Encontrar BAdI (SE18 o SAP Help)
2. Verificar que esté released
3. Implementar BAdI (SE19)
4. Activar implementation

**Ejemplo**:
```abap
CLASS zcl_standard_badi_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES zif_standard_badi.

ENDCLASS.

CLASS zcl_standard_badi_impl IMPLEMENTATION.

  METHOD zif_standard_badi~custom_validation.
    " Lógica de validación custom
    IF iv_value < '0'.
      RAISE EXCEPTION TYPE zcx_custom_error.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

### Pattern 3: Custom RAP Business Object

**Cuándo usar**: Crear nueva funcionalidad que no existe en estándar

**Pasos**:
1. Definir tablas de base de datos
2. Crear CDS Interface View
3. Crear CDS Consumption View
4. Definir Behavior Definition
5. Implementar Behavior Pool
6. Definir Service Definition
7. Crear Service Binding

**Estructura**:
```
Z_DT_INCT (Table)
  ↓
ZI_INCT (Interface CDS)
  ↓
ZC_INCT (Consumption CDS)
  ↓
Z_INCT_BDEF (Behavior Definition)
  ↓
ZBP_INCT (Behavior Pool)
  ↓
Z_INCT_SRV (Service Definition)
  ↓
Z_INCT_SRV_BND (Service Binding)
```

## Common Pitfalls

### Pitfall 1: Modifying Standard Code

**Error**: Modificar directamente código estándar de SAP

**Consecuencia**: Upgrade problems, loss of changes, no support

**Solución**: Usar extension points (BAdIs, CDS extensions, released APIs)

### Pitfall 2: Using Internal/Unreleased APIs

**Error**: Usar APIs o tablas que no están released

**Consecuencia**: Breaking changes, upgrade issues, no guaranteed compatibility

**Solución**: Usar solo released APIs (C1), check API_HUB

### Pitfall 3: Ignoring Deprecation Notices

**Error**: Seguir usando APIs deprecated sin migración

**Consecuencia**: APIs may be removed in future releases

**Solución**: Monitorear deprecation notices, plan migrations

### Pitfall 4: Hardcoding Values

**Error**: Hardcode valores en el código

**Consecuencia**: Difficult maintenance, environment-specific issues

**Solución**: Use configuration tables, parameters, or values from released APIs

### Pitfall 5: Skipping ATC Checks

**Error**: No correr ATC (ABAP Test Cockpit)

**Consecuencia**: Code quality issues, performance problems, security risks

**Solución**: Run ATC checks regularly, fix all issues before release

## Troubleshooting

### Issue: Extension Not Working

**Symptoms**: Extension not triggered, expected behavior not happening

**Possible Causes**:
1. Implementation not active
2. Filter values incorrect (if using filters)
3. Wrong BAdI/Extension point
4. Activation issues

**Solutions**:
1. Check implementation activation (SE19)
2. Verify filter conditions
3. Verify correct BAdI selected
4. Check activation status of all objects
5. Review logs for errors

### Issue: Released API Not Found

**Symptoms**: API not available in system

**Possible Causes**:
1. Wrong SAP version
2. Add-on not installed
3. API deprecated/removed
4. Authorization issues

**Solutions**:
1. Check SAP version compatibility
2. Install required add-ons
3. Search for alternative released API
4. Check authorizations (SU53)

### Issue: Upgrade Breaking Changes

**Symptoms**: Code breaks after SAP upgrade

**Possible Causes**:
1. Used deprecated APIs
2. Used internal tables/fields
3. Modified standard code
4. Hardcoded standard values

**Solutions**:
1. Review deprecated API notices
2. Migrate to released APIs
3. Use extensions instead of modifications
4. Remove hardcoding, use dynamic lookups

## References

### Official SAP Documentation
- SAP Help Portal: https://help.sap.com
- ABAP Cloud Programming Guide
- Clean Core Documentation
- SAP API Business Hub: https://api.sap.com

### Tools
- SE18: BAdI Builder
- SE19: BAdI Implementations
- SE80: Object Navigator
- API_HUB: Released API Browser
- ATC: ABAP Test Cockpit
- Custom Fields and Logic app

### Transactions
- SE18: Find BAdIs
- SE19: Implement BAdIs
- SE80: Workbench
- SLIN: Extended Program Check
- SAT: ABAP Performance Analyzer
- ST05: SQL Trace

---

*This skill provides a comprehensive foundation for understanding ABAP Cloud core concepts and Clean Core principles. Always refer to official SAP documentation for the most current information.*
