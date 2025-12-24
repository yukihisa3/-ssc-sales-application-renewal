****************************************************************
*    å⁄ãqñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@ÅiäîÅjÉTÉJÉ^ÇÃÉ^ÉlìaÅ@Å@Å@Å@Å@Å@Å@
*    ã∆ñ±ñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@ì`ï[ÇdÇwÇbÇdÇkòAåg
*    ÉÇÉWÉÖÅ[ÉãñºÅ@Å@Å@Å@ÅFÅ@ì`ï[ÇdÇwÇbÇdÇkéÊçûåèêîÉäÉXÉg
*    çÏê¨ì˙Å^çÏê¨é“Å@Å@Å@ÅFÅ@2016/09/16 INOUE
*    ó¨ópå≥Å@Å@Å@Å@Å@Å@Å@ÅFÅ@SZI0040L
*    èàóùäTóvÅ@Å@Å@Å@Å@Å@ÅFÅ@äeéÌéÊçûåèêîÇÉpÉâÉÅÅ[É^Ç…ÇƒÅ@
*    Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@  éÛéÊÇËÅAåèêîÉäÉXÉgÇèoóÕÇ∑ÇÈÅB
*    çXêVóöóÅ@Å@Å@Å@Å@Å@ÅF
*    Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SDE0040L.
 AUTHOR.                   NAV-ASSIST.
 DATE-WRITTEN.             2016/09/16.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YA-22       IS        YA-22
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*íSìñé“É}ÉXÉ^
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
* ÉvÉäÉìÉ^Å[
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*
*íSìñé“É}ÉXÉ^
 FD  HTANMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*ÉvÉäÉìÉ^Å[
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
*îNìx
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*
*ì˙ïtÅ^éûçè
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
     03  SYS-TIME   REDEFINES  WK-TIME.
         05  SYS-HH         PIC  9(02).
         05  SYS-MN         PIC  9(02).
         05  SYS-SS         PIC  9(02).
         05  SYS-MS         PIC  9(02).
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*ì˙ïtïœä∑ÉTÉuÉãÅ[É`ÉìópÉèÅ[ÉN
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  TAN-ERR           PIC N(10) VALUE
                        NC"íSìñé“É}ÉXÉ^ÉGÉâÅ[".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"ÉvÉäÉìÉ^Å[ÉGÉâÅ[".
*í†ï[ï\é¶ì˙ïtï“èW
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*í†ï[ï\é¶éûçèï“èW
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*ëﬁîÉGÉäÉA
 01  WK-BMNCD-BK           PIC  X(04).
 01  WK-TANCD-BK           PIC  X(02).
 01  WK-TANNM-BK           PIC  N(10).
 01  WK-TORCD-BK           PIC  X(08).
 01  WK-TORNM-BK           PIC  N(15).
*ïœä∑ÉGÉäÉA
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"ÇOÇPÇQÇRÇSÇTÇUÇVÇWÇXÅ@".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-KENSU-X.
     03  WK-KENSU          PIC  9(07) VALUE  ZERO.
 01  WK-HENKAN.
     03  WK-HENKAN-N       PIC  N(07).
     03  WK-HENKAN-NR  REDEFINES  WK-HENKAN-N
                           PIC  N(01)  OCCURS 7.
 01  WK-SHUKA-X.
     03  WK-SHUKA-G        PIC  9(07) VALUE  ZERO.
 01  WK-NYUKA-X.
     03  WK-NYUKA-G        PIC  9(07) VALUE  ZERO.
*çXêVîÕàÕ
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  IX                    PIC  9(04) VALUE 0.
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  FG-ZSOKMS-INV         PIC  X(03) VALUE SPACE.
 01  FG-HTANMS-INV         PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
 01  HENKAN-FLG            PIC  X(03) VALUE SPACE.
*í†ï[èoóÕíËã`ÉGÉäÉA
****  å©èoÇµçsÇP             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SDE0040L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"ÅÉì`ï[ÇdÇwÇbÇdÇkéÊçûåèêîÉäÉXÉgÅÑ".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"îN".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"åé".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"ì˙".
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"ï≈".
****  å©èoÇµçsÇQ***
 01  MIDASI-2.
     02  FILLER              PIC  X(116) VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"ÅF".
     02  TIMEMM              PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"ÅF".
     02  TIMESS              PIC  99.
****  ê¸ÇP           ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  ê¸ÇQ           ****
 01  HASEN-2.
     02  FILLER              PIC  X(27) VALUE  SPACE.
     02  FILLER              PIC  X(39) VALUE  ALL "-".

****  ñæç◊çsÇP               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(21)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"ì`ï[ÇdÇwÇbÇdÇkÉfÅ[É^ÇÃéÊçûÇçsÇ»Ç¢Ç‹ÇµÇΩÅB".
****  ñæç◊çsÇP-1             ****
 01  MEISAI-1-1.
     02  FILLER              PIC  X(44)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"Åi".
     02  JKUBUN              PIC  N(07)
         CHARACTER  TYPE  IS  YB-22      VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"Åj".
**** ñæç◊çsÇP-2             ****
 01  MEISAI-1-2.
     02  FILLER              PIC  X(28)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"ÇdÇwÇbÇdÇkéÌï ÅF".
     02  EXCEL               PIC  N(15)
         CHARACTER  TYPE  IS  YB-22      VALUE  SPACE.
**** ñæç◊çsÇQ               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(33) VALUE  SPACE.
     02  FILLER              PIC  N(05) VALUE
         NC"íSìñé“èÓïÒ"      CHARACTER  TYPE  IS  YB-21.
     02  FILLER              PIC  X(09) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
         NC"ÅF"              CHARACTER  TYPE  IS  YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  BMNCD               PIC  X(04).
     02  FILLER              PIC  X(01) VALUE  "-".
     02  TANCD               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TANNM               PIC  N(10) CHARACTER TYPE  IS  YB-21.
****  ñæç◊çsÇV               ****
 01  MEISAI-7.
     02  FILLER              PIC  X(17) VALUE  SPACE.
     02  FILLER              PIC  N(04) VALUE
         NC"éÊçûåèêî" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(04) VALUE  SPACE.
     02  FILLER              PIC  N(09) VALUE
         NC"éÊçûëçåèêîÅ@Å@Å@ÅF" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  KENSU01             PIC  N(07) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"åè"           CHARACTER TYPE IS YB-21.
****  ñæç◊çsÇW               ****
 01  MEISAI-8.
     02  FILLER              PIC  X(33) VALUE  SPACE.
     02  FILLER              PIC  N(09) VALUE
         NC"îÑè„ì`ï[åèêîÅ@Å@ÅF" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  KENSU02             PIC  N(07) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE NC"åè"
                                        CHARACTER TYPE IS YB-21.
****  ñæç◊çsÇX               ****
 01  MEISAI-9.
     02  FILLER              PIC  X(33) VALUE  SPACE.
     02  FILLER              PIC  N(09) VALUE
         NC"ï‘ïiì`ï[åèêîÅ@Å@ÅF" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  KENSU03             PIC  N(07) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE NC"åè"
                                        CHARACTER TYPE IS YB-21.
****  ñæç◊çs10               ****
 01  MEISAI-10.
     02  FILLER              PIC  X(33) VALUE  SPACE.
     02  FILLER              PIC  N(09) VALUE
         NC"ílà¯ì`ï[åèêîÅ@Å@ÅF" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  KENSU04             PIC  N(07) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE NC"åè"
                                        CHARACTER TYPE IS YB-21.
****  ñæç◊çs11               ****
 01  MEISAI-11.
     02  FILLER              PIC  X(33) VALUE  SPACE.
     02  FILLER              PIC  N(09) VALUE
         NC"ÉGÉâÅ[Å@åèêîÅ@Å@ÅF" CHARACTER TYPE  IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  KENSU05             PIC  N(07) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE NC"åè"
                                        CHARACTER TYPE IS YB-21.
*
 LINKAGE                   SECTION.
 01  PAR-IN-JKUBUN           PIC X(01).
 01  PAR-IN-EXCEL            PIC X(01).
 01  PAR-IN-BMNCD            PIC X(04).
 01  PAR-IN-TANCD            PIC X(02).
 01  PAR-IN-KENSU01          PIC X(07).
 01  PAR-IN-KENSU02          PIC X(07).
 01  PAR-IN-KENSU03          PIC X(07).
 01  PAR-IN-KENSU04          PIC X(07).
 01  PAR-IN-KENSU05          PIC X(07).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-IN-JKUBUN
                                          PAR-IN-EXCEL
                                          PAR-IN-BMNCD
                                          PAR-IN-TANCD
                                          PAR-IN-KENSU01
                                          PAR-IN-KENSU02
                                          PAR-IN-KENSU03
                                          PAR-IN-KENSU04
                                          PAR-IN-KENSU05.
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ÇlÇ`ÇhÇmèàóù                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Çh Çm Çh Çs                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*
*ÉVÉXÉeÉÄì˙ïtÅEéûçèÇÃéÊìæ
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*ÉVÉXÉeÉÄéûçèéÊìæ
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     HTANMS
                 OUTPUT    PRTF.
*
     DISPLAY  "*** SDE0040L START *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.

*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ÇlÇ`ÇhÇmèàóù                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     PERFORM     HEAD-SET-SEC.
     PERFORM     MEIEDT-SEC.
     PERFORM     MIDA-SEC.
     PERFORM     MEIWRT-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Çd Çm Çc                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF   HTANMS.
 END-EXIT.
     EXIT.
**********************************************************
*                 ÉwÉbÉ_ÉfÅ[É^ï“èW                      *
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     MOVE        WK-TIME(1:2)  TO    TIMEHH.
     MOVE        WK-TIME(3:2)  TO    TIMEMM.
     MOVE        WK-TIME(5:2)  TO    TIMESS.
*
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*                 å©èoÇµÉfÅ[É^ï“èWèëÇ´èoÇµ               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*í†ï[èëÇ´èoÇµ*
**************
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    HASEN-1    AFTER  1.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 ñæç◊èÓïÒÇÃï“èW                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*íSìñé“
     MOVE        PAR-IN-BMNCD   TO    BMNCD   TAN-F01.
     MOVE        PAR-IN-TANCD   TO    TANCD   TAN-F02.
     PERFORM     RD-HTANMS-SEC.
     IF FG-HTANMS-INV = ZERO
        MOVE  TAN-F03      TO  TANNM
     END-IF.
*é¿çsãÊï™
     EVALUATE PAR-IN-JKUBUN
        WHEN  "1"
          MOVE NC"É`ÉFÉbÉNÇÃÇ›Å@" TO JKUBUN
        WHEN  "2"
          MOVE NC"É`ÉFÉbÉNÅ`çXêV" TO JKUBUN
        WHEN  OTHER
          MOVE NC"ÅñÅñÅñÅñÅñÅñÅñ" TO JKUBUN
     END-EVALUATE.
*ÇdÇwÇbÇdÇkéÌï 
     EVALUATE PAR-IN-EXCEL
        WHEN  "1"
          MOVE NC"ì`ï[ì¸óÕå`éÆÅQì`ï[î‘çÜéwíË"     TO EXCEL
        WHEN  "2"
          MOVE NC"ì`ï[ì¸óÕå`éÆÅQì`ï[î‘çÜé©ìÆçÃî‘" TO EXCEL
        WHEN  "3"
          MOVE NC"ó îÃìXì¸óÕå`éÆÅQì`ï[é©ìÆçÃî‘"   TO EXCEL
        WHEN  OTHER
          MOVE NC"ÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñÅñ" TO EXCEL
     END-EVALUATE.
*éÊçûåèêî
*   ëçåèêî
     MOVE     PAR-IN-KENSU01  TO  WK-KENSU.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-HENKAN-N  TO  KENSU01.
*   îÑè„ì`ï[åèêî
     MOVE     PAR-IN-KENSU02  TO  WK-KENSU.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-HENKAN-N  TO  KENSU02.
*   ï‘ïiì`ï[åèêîÅ@Å@
     MOVE     PAR-IN-KENSU03  TO  WK-KENSU.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-HENKAN-N  TO  KENSU03.
*   ílà¯ì`ï[åèêîÅ@
     MOVE     PAR-IN-KENSU04  TO  WK-KENSU.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-HENKAN-N  TO  KENSU04.
*   ÉGÉâÅ[åèêî
     MOVE     PAR-IN-KENSU05  TO  WK-KENSU.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-HENKAN-N  TO  KENSU05.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    ñæç◊ÉfÅ[É^èëÇ´èoÇµ                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
*    ADD        1          TO        IN-CNT.
**************
*í†ï[èëÇ´èoÇµ*
**************
     WRITE      P-REC   FROM    MEISAI-1   AFTER  5.
     WRITE      P-REC   FROM    MEISAI-1-1 AFTER  2.
     WRITE      P-REC   FROM    MEISAI-1-2 AFTER  3.
     WRITE      P-REC   FROM    HASEN-1    AFTER  3.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  2.
     WRITE      P-REC   FROM    HASEN-1    AFTER  2.
     WRITE      P-REC   FROM    MEISAI-7   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-8   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-9   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-10  AFTER  2.
     WRITE      P-REC   FROM    MEISAI-11  AFTER  2.
*
*
*    ADD         4         TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*    íSìñé“É}ÉXÉ^åüçı                                          *
****************************************************************
 RD-HTANMS-SEC          SECTION.

     READ  HTANMS
       INVALID
         MOVE  1                 TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTANMS-INV
     END-READ.

 RD-HTANMS-EXIT.
     EXIT.
****************************************************************
*    ëSäpïœä∑Å@Å@Å@                                          *
****************************************************************
 ZENKAKU-SEC          SECTION.
* ì˙ñ{åÍïœä∑Ç∑ÇÈÅB
     MOVE   SPACE  TO  HENKAN-FLG.
     PERFORM  VARYING IX  FROM 1 BY 1 UNTIL   IX > 7
     IF ( WK-KENSU-X(IX:1) NOT = 0 ) AND
        ( WK-KENSU-X(IX:1) NOT = SPACE )
         MOVE "SET"   TO  HENKAN-FLG
     END-IF
         EVALUATE    WK-KENSU-X(IX:1)
             WHEN    0
                 IF HENKAN-FLG = "SET"
                     MOVE NC"ÇO"  TO   WK-HENKAN-NR(IX)
                 ELSE
                     MOVE NC"Å@"  TO   WK-HENKAN-NR(IX)
                 END-IF
             WHEN    1
                 MOVE NC"ÇP"  TO   WK-HENKAN-NR(IX)
             WHEN    2
                 MOVE NC"ÇQ"  TO   WK-HENKAN-NR(IX)
             WHEN    3
                 MOVE NC"ÇR"  TO   WK-HENKAN-NR(IX)
             WHEN    4
                 MOVE NC"ÇS"  TO   WK-HENKAN-NR(IX)
             WHEN    5
                 MOVE NC"ÇT"  TO   WK-HENKAN-NR(IX)
             WHEN    6
                 MOVE NC"ÇU"  TO   WK-HENKAN-NR(IX)
             WHEN    7
                 MOVE NC"ÇV"  TO   WK-HENKAN-NR(IX)
             WHEN    8
                 MOVE NC"ÇW"  TO   WK-HENKAN-NR(IX)
             WHEN    9
                 MOVE NC"ÇX"  TO   WK-HENKAN-NR(IX)
         END-EVALUATE
     END-PERFORM.
 ZENKAKU-EXIT.
     EXIT.
