<map version="freeplane 1.3.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="Redo" ID="ID_1723255651" CREATED="1283093380553" MODIFIED="1519240644161"><hook NAME="MapStyle">
    <properties show_icon_for_attributes="true"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node">
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right">
<stylenode LOCALIZED_TEXT="default" MAX_WIDTH="600" COLOR="#000000" STYLE="as_parent">
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.note"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important">
<icon BUILTIN="yes"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="8"/>
<node TEXT="Integration" POSITION="right" ID="ID_540637557" CREATED="1519240142196" MODIFIED="1519240644160" HGAP="40" VSHIFT="-50">
<edge COLOR="#00ff00"/>
<node TEXT="Single files" ID="ID_353006866" CREATED="1519240533779" MODIFIED="1519240616831" VSHIFT="10">
<node TEXT="Single source file detected" ID="ID_1913540470" CREATED="1519240484937" MODIFIED="1519240503959"/>
<node TEXT="Single file generated" ID="ID_1473979625" CREATED="1519240505097" MODIFIED="1519240512319"/>
<node TEXT="Single file updated when single source changes" ID="ID_1424360526" CREATED="1519240513682" MODIFIED="1519240523368"/>
<node TEXT="Single file updated when script changes" ID="ID_603485006" CREATED="1519240524018" MODIFIED="1519240532919"/>
</node>
<node TEXT="Linear dependency chain" ID="ID_777244117" CREATED="1519240581860" MODIFIED="1519240635845" VSHIFT="-30"/>
</node>
<node TEXT="Sandbox" POSITION="left" ID="ID_1895927120" CREATED="1519090988184" MODIFIED="1519240426632" HGAP="50" VSHIFT="-40">
<edge COLOR="#ff00ff"/>
<node TEXT="Load from Env" ID="ID_1839174331" CREATED="1519099272423" MODIFIED="1519099277405"/>
<node TEXT="FInd in ancestor dir" ID="ID_1237260432" CREATED="1519099277760" MODIFIED="1519099291189"/>
</node>
<node TEXT="Configure" POSITION="right" ID="ID_927061733" CREATED="1519091034414" MODIFIED="1519240465258" HGAP="80" VSHIFT="-20">
<edge COLOR="#00ffff"/>
</node>
<node TEXT="Build Log" POSITION="right" ID="ID_792684292" CREATED="1519092679429" MODIFIED="1519240431320" HGAP="50" VSHIFT="40">
<edge COLOR="#ffff00"/>
<node TEXT="Script Result" ID="ID_160433501" CREATED="1519092703062" MODIFIED="1519099383700" VSHIFT="10"/>
<node TEXT="Dependency" ID="ID_1645750612" CREATED="1519092728200" MODIFIED="1519098654078" VSHIFT="10">
<node TEXT="External" ID="ID_1757268052" CREATED="1519092738063" MODIFIED="1519092745685"/>
<node TEXT="Source" ID="ID_1961126532" CREATED="1519092746679" MODIFIED="1519092754045"/>
<node TEXT="Generated" ID="ID_1397382063" CREATED="1519092755064" MODIFIED="1519092765252"/>
<node TEXT="Script" ID="ID_1631113505" CREATED="1519092765776" MODIFIED="1519092768150"/>
</node>
</node>
<node TEXT="Script Management" POSITION="left" ID="ID_565843197" CREATED="1519092776992" MODIFIED="1519240432844" HGAP="40" VSHIFT="50">
<edge COLOR="#7c0000"/>
<node TEXT="Setup Environment" ID="ID_1226866145" CREATED="1519098673171" MODIFIED="1519098691713"/>
<node TEXT="Manage Tmp Files" ID="ID_1890767909" CREATED="1519098693172" MODIFIED="1519101880631" VSHIFT="10">
<node TEXT="Create" ID="ID_1140696178" CREATED="1519098890882" MODIFIED="1519098899297"/>
<node TEXT="Move into place" ID="ID_1399165037" CREATED="1519098900531" MODIFIED="1519098939529"/>
<node TEXT="Cleanup" ID="ID_1855652169" CREATED="1519098918179" MODIFIED="1519098924393"/>
</node>
<node TEXT="Monitor" ID="ID_38328311" CREATED="1519098842633" MODIFIED="1519098863127">
<node TEXT="Output" ID="ID_60064854" CREATED="1519098864042" MODIFIED="1519098869727"/>
<node TEXT="Exit code" ID="ID_1875895271" CREATED="1519098870370" MODIFIED="1519098876151"/>
</node>
</node>
</node>
</map>
