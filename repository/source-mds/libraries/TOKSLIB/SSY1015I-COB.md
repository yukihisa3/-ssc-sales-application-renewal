# SSY1015I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY1015I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理                          *
*    業務名　　　　　　　：　手書伝票データ抽出                *
*    モジュール名　　　　：　手書伝票データ抽出指示入力        *
*    作成日／更新日　　　：　99/09/21                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、倉庫、抽出取引先、抽    *
*                            出伝票_範囲を入力後、チェック    *
*                            し、パラメタへ渡す。              *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY0015I.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/20.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY00151  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  SONZAI-FLG     PIC  9(01)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
*
 01  IX                PIC  9(02)      VALUE  ZERO.
 01  IY                PIC  9(02)      VALUE  ZERO.
 01  IZ                PIC  9(02)      VALUE  ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"倉庫コードを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"倉庫コードが違います。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(20)  VALUE
              NC"売上伝票データに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"範囲指定が違います。".
     03  MSG07               PIC  N(20)  VALUE
              NC"取引先コードを入力して下さい。".
     03  MSG08               PIC  N(20)  VALUE
              NC"取引先コードが重複しています。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       8.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SOKO              PIC   X(02).
 01  PARA-TORICD01          PIC   9(08).
 01  PARA-SDENNO01          PIC   9(09).
 01  PARA-EDENNO01          PIC   9(09).
 01  PARA-TORICD02          PIC   9(08).
 01  PARA-SDENNO02          PIC   9(09).
 01  PARA-EDENNO02          PIC   9(09).
 01  PARA-TORICD03          PIC   9(08).
 01  PARA-SDENNO03          PIC   9(09).
 01  PARA-EDENNO03          PIC   9(09).
 01  PARA-TORICD04          PIC   9(08).
 01  PARA-SDENNO04          PIC   9(09).
 01  PARA-EDENNO04          PIC   9(09).
 01  PARA-TORICD05          PIC   9(08).
 01  PARA-SDENNO05          PIC   9(09).
 01  PARA-EDENNO05          PIC   9(09).
 01  PARA-TORICD06          PIC   9(08).
 01  PARA-SDENNO06          PIC   9(09).
 01  PARA-EDENNO06          PIC   9(09).
 01  PARA-TORICD07          PIC   9(08).
 01  PARA-SDENNO07          PIC   9(09).
 01  PARA-EDENNO07          PIC   9(09).
 01  PARA-TORICD08          PIC   9(08).
 01  PARA-SDENNO08          PIC   9(09).
 01  PARA-EDENNO08          PIC   9(09).
 01  PARA-TORICD09          PIC   9(08).
 01  PARA-SDENNO09          PIC   9(09).
 01  PARA-EDENNO09          PIC   9(09).
 01  PARA-TORICD10          PIC   9(08).
 01  PARA-SDENNO10          PIC   9(09).
 01  PARA-EDENNO10          PIC   9(09).
 01  PARA-TORICD11          PIC   9(08).
 01  PARA-SDENNO11          PIC   9(09).
 01  PARA-EDENNO11          PIC   9(09).
 01  PARA-TORICD12          PIC   9(08).
 01  PARA-SDENNO12          PIC   9(09).
 01  PARA-EDENNO12          PIC   9(09).
 01  PARA-DSOKO             PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING      PARA-SOKO
               PARA-TORICD01  PARA-SDENNO01  PARA-EDENNO01
               PARA-TORICD02  PARA-SDENNO02  PARA-EDENNO02
               PARA-TORICD03  PARA-SDENNO03  PARA-EDENNO03
               PARA-TORICD04  PARA-SDENNO04  PARA-EDENNO04
               PARA-TORICD05  PARA-SDENNO05  PARA-EDENNO05
               PARA-TORICD06  PARA-SDENNO06  PARA-EDENNO06
               PARA-TORICD07  PARA-SDENNO07  PARA-EDENNO07
               PARA-TORICD08  PARA-SDENNO08  PARA-EDENNO08
               PARA-TORICD09  PARA-SDENNO09  PARA-EDENNO09
               PARA-TORICD10  PARA-SDENNO10  PARA-EDENNO10
               PARA-TORICD11  PARA-SDENNO11  PARA-EDENNO11
               PARA-TORICD12  PARA-SDENNO12  PARA-EDENNO12
               PARA-DSOKO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0015I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0015I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0015I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0015I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ｿｳｺｺｰﾄﾞ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾄﾘﾋｷｻｷ ﾃﾞﾝﾋﾟｮｳﾊﾝｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0015I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSY00151.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY0015I"      TO   PGID.
     MOVE    "FSY00151"      TO   FORM.
*
     IF      PARA-DSOKO  =  "01"
             MOVE      1         TO   GR-NO
     ELSE
             MOVE      PARA-SOKO TO   SOKO
             MOVE      SOKO      TO   SOK-F01
             READ      ZSOKMS1
                 INVALID  KEY
                       MOVE      SPACE     TO   SOKONM
             NOT INVALID  KEY
                       MOVE      SOK-F02   TO   SOKONM
             END-READ
             MOVE      2         TO   GR-NO
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY00151"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｿｳｺｺｰﾄﾞ  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｿｳｺｺｰﾄﾞ ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
*    倉庫コードチェック
*****IF     ( SOKO      IS   NUMERIC   )   AND
************( SOKO      NOT =     ZERO )
     IF     ( SOKO      NOT =     SPACE )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        MOVE      3    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKONM
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF SOKO
              MOVE     "R"        TO   EDIT-OPTION OF SOKO
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾄﾘﾋｷｻｷ ﾃﾞﾝﾋﾟｮｳNO.ﾊﾝｲ    ﾆｭｳﾘｮｸ              *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              IF  PARA-DSOKO = "01"
                  MOVE      1         TO   GR-NO
              END-IF
         WHEN ENT
              PERFORM   CLR-BODY-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾄﾘﾋｷｻｷ ﾃﾞﾝﾋﾟｮｳNO.ﾊﾝｲ ﾁｪｯｸ                   *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
     MOVE     ZERO      TO        SONZAI-FLG.
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
*        抽出取引先
         IF ( TORICD(IX)     IS NUMERIC     )    AND
            ( TORICD(IX)     NOT =     ZERO )
              MOVE      1              TO   SONZAI-FLG
              MOVE      SPACE          TO   TOK-REC
              INITIALIZE                    TOK-REC
              MOVE      TORICD(IX)     TO   TOK-F01
              READ      HTOKMS
                   INVALID
                        MOVE      SPACE     TO   TORINM(IX)
                        MOVE  4   TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF TORICD(IX)
                        MOVE "R"  TO   EDIT-OPTION OF TORICD(IX)
                   NOT INVALID
                        MOVE      TOK-F02   TO   TORINM(IX)
              END-READ
         ELSE
              MOVE      SPACE     TO        TORINM(IX)
         END-IF
*        抽出伝票_範囲
         IF ( SDENNO(IX)     IS   NUMERIC   AND
              EDENNO(IX)     IS   NUMERIC )
              IF   SDENNO(IX)     >    EDENNO(IX)
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF SDENNO(IX)
                   MOVE     "R"   TO   EDIT-OPTION OF SDENNO(IX)
                   MOVE     "R"   TO   EDIT-OPTION OF EDENNO(IX)
              ELSE
                   IF   TORICD(IX)     IS NOT NUMERIC
                        IF   ERR-FLG   =    ZERO
                             MOVE      7    TO   ERR-FLG
                        END-IF
                        MOVE "C"  TO   EDIT-CURSOR OF TORICD(IX)
                        MOVE "R"  TO   EDIT-OPTION OF TORICD(IX)
                   END-IF
              END-IF
         END-IF
     END-PERFORM.
*
*    抽出取引先重複チェック
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  11
         IF ( TORICD(IX)     IS NUMERIC     )    AND
            ( TORICD(IX)     NOT =     ZERO )
              COMPUTE   IZ   =     IX  +  1
              PERFORM   VARYING    IY  FROM  IZ  BY  1
                                       UNTIL IY  >   12
                   IF ( TORICD(IY)     IS NUMERIC     )  AND
                      ( TORICD(IY)     NOT =     ZERO )
                        IF   TORICD(IX)     =    TORICD(IY)
                             IF   ERR-FLG   =    ZERO
                                  MOVE      8    TO   ERR-FLG
                                  MOVE "C"  TO
                                       EDIT-CURSOR OF TORICD(IX)
                             END-IF
                             MOVE "R"  TO
                                       EDIT-OPTION OF TORICD(IX)
                                       EDIT-OPTION OF TORICD(IY)
                        END-IF
                   END-IF
              END-PERFORM
         END-IF
     END-PERFORM.
*    明細未入力チェック
     IF  ERR-FLG   =    ZERO
         IF   SONZAI-FLG      =   ZERO
              MOVE  7   TO   ERR-FLG
              MOVE "C"  TO   EDIT-CURSOR OF TORICD(1)
         END-IF
     END-IF.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      2         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     SOKO                     TO   PARA-SOKO.
     IF  TORICD(1)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD01
         MOVE      ZERO           TO   PARA-SDENNO01
         MOVE      ZERO           TO   PARA-EDENNO01
     ELSE
         MOVE      TORICD(1)      TO   PARA-TORICD01
         IF   SDENNO(1)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO01
         ELSE
              MOVE      SDENNO(1)      TO   PARA-SDENNO01
         END-IF
         IF   EDENNO(1)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO01
         ELSE
              MOVE      EDENNO(1)      TO   PARA-EDENNO01
         END-IF
     END-IF.
*
     IF  TORICD(2)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD02
         MOVE      ZERO           TO   PARA-SDENNO02
         MOVE      ZERO           TO   PARA-EDENNO02
     ELSE
         MOVE      TORICD(2)      TO   PARA-TORICD02
         IF   SDENNO(2)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO02
         ELSE
              MOVE      SDENNO(2)      TO   PARA-SDENNO02
         END-IF
         IF   EDENNO(2)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO02
         ELSE
              MOVE      EDENNO(2)      TO   PARA-EDENNO02
         END-IF
     END-IF.
*
     IF  TORICD(3)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD03
         MOVE      ZERO           TO   PARA-SDENNO03
         MOVE      ZERO           TO   PARA-EDENNO03
     ELSE
         MOVE      TORICD(3)      TO   PARA-TORICD03
         IF   SDENNO(3)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO03
         ELSE
              MOVE      SDENNO(3)      TO   PARA-SDENNO03
         END-IF
         IF   EDENNO(3)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO03
         ELSE
              MOVE      EDENNO(3)      TO   PARA-EDENNO03
         END-IF
     END-IF.
*
     IF  TORICD(4)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD04
         MOVE      ZERO           TO   PARA-SDENNO04
         MOVE      ZERO           TO   PARA-EDENNO04
     ELSE
         MOVE      TORICD(4)      TO   PARA-TORICD04
         IF   SDENNO(4)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO04
         ELSE
              MOVE      SDENNO(4)      TO   PARA-SDENNO04
         END-IF
         IF   EDENNO(4)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO04
         ELSE
              MOVE      EDENNO(4)      TO   PARA-EDENNO04
         END-IF
     END-IF.
*
     IF  TORICD(5)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD05
         MOVE      ZERO           TO   PARA-SDENNO05
         MOVE      ZERO           TO   PARA-EDENNO05
     ELSE
         MOVE      TORICD(5)      TO   PARA-TORICD05
         IF   SDENNO(5)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO05
         ELSE
              MOVE      SDENNO(5)      TO   PARA-SDENNO05
         END-IF
         IF   EDENNO(5)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO05
         ELSE
              MOVE      EDENNO(5)      TO   PARA-EDENNO05
         END-IF
     END-IF.
*
     IF  TORICD(6)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD06
         MOVE      ZERO           TO   PARA-SDENNO06
         MOVE      ZERO           TO   PARA-EDENNO06
     ELSE
         MOVE      TORICD(6)      TO   PARA-TORICD06
         IF   SDENNO(6)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO06
         ELSE
              MOVE      SDENNO(6)      TO   PARA-SDENNO06
         END-IF
         IF   EDENNO(6)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO06
         ELSE
              MOVE      EDENNO(6)      TO   PARA-EDENNO06
         END-IF
     END-IF.
*
     IF  TORICD(7)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD07
         MOVE      ZERO           TO   PARA-SDENNO07
         MOVE      ZERO           TO   PARA-EDENNO07
     ELSE
         MOVE      TORICD(7)      TO   PARA-TORICD07
         IF   SDENNO(7)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO07
         ELSE
              MOVE      SDENNO(7)      TO   PARA-SDENNO07
         END-IF
         IF   EDENNO(7)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO07
         ELSE
              MOVE      EDENNO(7)      TO   PARA-EDENNO07
         END-IF
     END-IF.
*
     IF  TORICD(8)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD08
         MOVE      ZERO           TO   PARA-SDENNO08
         MOVE      ZERO           TO   PARA-EDENNO08
     ELSE
         MOVE      TORICD(8)      TO   PARA-TORICD08
         IF   SDENNO(8)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO08
         ELSE
              MOVE      SDENNO(8)      TO   PARA-SDENNO08
         END-IF
         IF   EDENNO(8)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO08
         ELSE
              MOVE      EDENNO(8)      TO   PARA-EDENNO08
         END-IF
     END-IF.
*
     IF  TORICD(9)      NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD09
         MOVE      ZERO           TO   PARA-SDENNO09
         MOVE      ZERO           TO   PARA-EDENNO09
     ELSE
         MOVE      TORICD(9)      TO   PARA-TORICD09
         IF   SDENNO(9)      NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO09
         ELSE
              MOVE      SDENNO(9)      TO   PARA-SDENNO09
         END-IF
         IF   EDENNO(9)      NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO09
         ELSE
              MOVE      EDENNO(9)      TO   PARA-EDENNO09
         END-IF
     END-IF.
*
     IF  TORICD(10)     NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD10
         MOVE      ZERO           TO   PARA-SDENNO10
         MOVE      ZERO           TO   PARA-EDENNO10
     ELSE
         MOVE      TORICD(10)     TO   PARA-TORICD10
         IF   SDENNO(10)     NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO10
         ELSE
              MOVE      SDENNO(10)     TO   PARA-SDENNO10
         END-IF
         IF   EDENNO(10)     NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO10
         ELSE
              MOVE      EDENNO(10)     TO   PARA-EDENNO10
         END-IF
     END-IF.
*
     IF  TORICD(11)     NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD11
         MOVE      ZERO           TO   PARA-SDENNO11
         MOVE      ZERO           TO   PARA-EDENNO11
     ELSE
         MOVE      TORICD(11)     TO   PARA-TORICD11
         IF   SDENNO(11)     NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO11
         ELSE
              MOVE      SDENNO(11)     TO   PARA-SDENNO11
         END-IF
         IF   EDENNO(11)     NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO11
         ELSE
              MOVE      EDENNO(11)     TO   PARA-EDENNO11
         END-IF
     END-IF.
*
     IF  TORICD(12)     NOT NUMERIC
         MOVE      ZERO           TO   PARA-TORICD12
         MOVE      ZERO           TO   PARA-SDENNO12
         MOVE      ZERO           TO   PARA-EDENNO12
     ELSE
         MOVE      TORICD(12)     TO   PARA-TORICD12
         IF   SDENNO(12)     NOT NUMERIC
              MOVE      ZERO           TO   PARA-SDENNO12
         ELSE
              MOVE      SDENNO(12)     TO   PARA-SDENNO12
         END-IF
         IF   EDENNO(12)     NOT NUMERIC
              MOVE      999999999      TO   PARA-EDENNO12
         ELSE
              MOVE      EDENNO(12)     TO   PARA-EDENNO12
         END-IF
     END-IF.
*
**QQ MOVE     0                   TO   GR-NO.
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*    行_ｾｯﾄ
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
         MOVE      IX        TO   BANGO(IX)
     END-PERFORM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY00151.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SOKO.
     MOVE     "M"            TO   EDIT-OPTION OF SOKO.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
*
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
         MOVE     " "        TO   EDIT-CURSOR OF TORICD(IX)
                                  EDIT-CURSOR OF SDENNO(IX)
                                  EDIT-CURSOR OF EDENNO(IX)
         MOVE     "M"        TO   EDIT-OPTION OF TORICD(IX)
                                  EDIT-OPTION OF SDENNO(IX)
                                  EDIT-OPTION OF EDENNO(IX)
     END-PERFORM.
*
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
