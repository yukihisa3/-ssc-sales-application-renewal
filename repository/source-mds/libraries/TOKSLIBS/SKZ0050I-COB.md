# SKZ0050I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKZ0050I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　出荷依頼リスト発行指示            *
*    作成日／更新日　　　：　2011/04/22                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より出荷依頼リスト発行を行な　*
*                            う条件を入力する。　　　　　　　  *
*                                                              *
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKZ0050I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/04/22.
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
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
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
     COPY     FKZ00501  OF        XMDLIB.
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
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
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HTANMS-ST         PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
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
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
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
 01  GUIDE01       PIC  N(40)  VALUE
         NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"倉庫ＣＤ未登録です。".
     03  MSG03               PIC  N(20)  VALUE
              NC"開始が終了を超えています。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先ＣＤ未登録です。".
     03  MSG05               PIC  N(20)  VALUE
              NC"納品日を正しく入力して下さい。".
     03  MSG06               PIC  N(20)  VALUE
              NC"作成日を正しく入力して下さい。".
     03  MSG07               PIC  N(20)  VALUE
              NC"担当者ＣＤ未登録です。".
     03  MSG08               PIC  N(20)  VALUE
              NC"ＤＴ区分を正しく入力してください。".
     03  MSG09               PIC  N(20)  VALUE
              NC"開始納品日は必ず指定して下さい。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       9.
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
 01  PARA-BUMON             PIC   X(04).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-DSOKO             PIC   X(02).
 01  PARA-SNOUDT            PIC   9(08).
 01  PARA-ENOUDT            PIC   9(08).
 01  PARA-SSAKUSEI          PIC   9(08).
 01  PARA-ESAKUSEI          PIC   9(08).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-DTKBN             PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-BUMON
                                       PARA-SOKO
                                       PARA-DSOKO
                                       PARA-SNOUDT
                                       PARA-ENOUDT
                                       PARA-SSAKUSEI
                                       PARA-ESAKUSEI
                                       PARA-TORICD
                                       PARA-TANCD
                                       PARA-DTKBN.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKZ0050I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTANMS   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 HTANMS-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKZ0050I TANMS1 ERROR " HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTANMS   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKZ0050I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTANMS   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKZ0050I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTANMS   HTOKMS    ZSOKMS1    DSPFILE.
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
     DISPLAY  "*** SKZ0050I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTANMS.
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
*----<< 初期画面 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< 発行条件入力 >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< 確認入力 >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< パラメタセット >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    HTANMS.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKZ0050I END *** "
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
     MOVE     SPACE          TO   FKZ00501.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SKZ0050I"      TO   PGID.
     MOVE    "FKZ00501"      TO   FORM.
*納品日開始セット
     MOVE    SYS-DATEW       TO   SNOUDT.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01" OR "88"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01 SOKO
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKONM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKONM
              END-READ
**************プロテクト追加
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
              MOVE    "X"    TO   EDIT-STATUS OF SCRTDT
              MOVE    "X"    TO   EDIT-STATUS OF ECRTDT
              MOVE    "X"    TO   EDIT-STATUS OF TANCD
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FKZ00501"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    倉庫コードチェック
     IF     ( SOKO      NOT =     SPACE )
     AND    ( SOKO      NOT =     "00"  )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
                        MOVE      SOKO      TO   WK-SOKCD
              END-READ
     ELSE
              MOVE      SPACE          TO   WK-SOKCD
              MOVE      NC"全倉庫"     TO   SOKONM
     END-IF.
*開始納品日チェック
     IF       SNOUDT   NOT NUMERIC
     OR       SNOUDT   =   ZERO
              MOVE     ZERO  TO   SNOUDT
     END-IF.
*開始納品日論理チェック
     IF       SNOUDT  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     SNOUDT         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   IF   ERR-FLG   =    ZERO
                        MOVE      5    TO   ERR-FLG
                   END-IF
                   MOVE  "R"      TO   EDIT-OPTION  OF  SNOUDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  SNOUDT
              ELSE
*******************開始日が入力されて終了日が未入力の場合
*******************開始日＋３日を算出して終了日とする。
                   IF  ENOUDT   NOT NUMERIC
                   OR  ENOUDT = ZERO
                       MOVE ZERO       TO ENOUDT
                       MOVE "5"        TO LINK-IN-KBN
                       MOVE  3         TO LINK-IN-YMD6
                       MOVE SNOUDT     TO LINK-IN-YMD8
                       MOVE ZERO       TO LINK-OUT-RET
                       MOVE ZERO       TO LINK-OUT-YMD8
                       CALL "SKYDTCKB" USING LINK-IN-KBN
                                             LINK-IN-YMD6
                                             LINK-IN-YMD8
                                             LINK-OUT-RET
                                             LINK-OUT-YMD8
                       MOVE LINK-OUT-YMD8 TO ENOUDT
                   END-IF
              END-IF
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE      9    TO   ERR-FLG
              END-IF
              MOVE     "R"   TO   EDIT-OPTION OF SNOUDT
              MOVE     "C"   TO   EDIT-CURSOR OF SNOUDT
     END-IF.
*終了納品日論理チェック
     IF       ENOUDT  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     ENOUDT         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   IF   ERR-FLG   =    ZERO
                        MOVE      5    TO   ERR-FLG
                   END-IF
                   MOVE  "R"      TO   EDIT-OPTION  OF  ENOUDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  ENOUDT
              END-IF
     END-IF.
*納品日大小チェック
     IF       SNOUDT  >  ENOUDT
              IF   ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  SNOUDT
              MOVE  "R"      TO   EDIT-OPTION  OF  ENOUDT
              MOVE  "C"      TO   EDIT-CURSOR  OF  SNOUDT
     END-IF.
*ＤＴ作成日開始チェック
*    （開始ＤＴ作成日チェック）
     IF       SCRTDT   NOT NUMERIC
     OR       SCRTDT   =   ZERO
              MOVE     ZERO  TO   SCRTDT
     END-IF.
*    （終了ＤＴ作成日チェック）
     IF       ECRTDT   NOT NUMERIC
     OR       ECRTDT   =   ZERO
              MOVE  99999999 TO   ECRTDT
     END-IF.
*    開始ＤＴ作成日論理チェック
     IF       SCRTDT   NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     SCRTDT         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE  "R"      TO   EDIT-OPTION  OF  SCRTDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  SCRTDT
              END-IF
     END-IF.
*    終了納品日論理チェック
     IF       ECRTDT   NOT =  99999999
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     ECRTDT         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE  "R"      TO   EDIT-OPTION  OF  ECRTDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  ECRTDT
              END-IF
     END-IF.
*納品日入力範囲チェック
     IF       SCRTDT      >  ECRTDT
              MOVE   3       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  SCRTDT
              MOVE  "R"      TO   EDIT-OPTION  OF  ECRTDT
              MOVE  "C"      TO   EDIT-CURSOR  OF  SCRTDT
     END-IF.
*取引先チェック
     IF  TORICD  NOT  NUMERIC
         MOVE      ZERO      TO   TORICD
     END-IF.
*取引先マスタチェック
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
             NOT INVALID
                   MOVE      TOK-F02   TO   TORINM
         END-READ
     ELSE
         MOVE      NC"全取引先対象"    TO   TORINM
     END-IF.
*担当者マスタチェック
     IF  TANCD     NOT =     SPACE
         MOVE      SPACE     TO   TAN-REC
         INITIALIZE               TAN-REC
         MOVE    PARA-BUMON  TO   TAN-F01
         MOVE      TANCD     TO   TAN-F02
         READ      HTANMS
             INVALID
                   MOVE      SPACE     TO   TANNM
                   IF   ERR-FLG   =    ZERO
                        MOVE      7    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TANCD
                   MOVE     "R"   TO   EDIT-OPTION OF TANCD
             NOT INVALID
                   MOVE      TAN-F03   TO   TANNM
         END-READ
     ELSE
         MOVE      NC"全担当者対象"    TO   TANNM
     END-IF.
*ＤＴ区分チェック
     IF  DTKBN  =  SPACE OR "1" OR "2"
         CONTINUE
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      8    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TANCD
         MOVE     "R"   TO   EDIT-OPTION OF TANCD
     END-IF.
*
 220-GRP01-CHECK-EXIT.
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
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
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
     MOVE     SOKO                TO   PARA-SOKO.
     MOVE     SNOUDT              TO   PARA-SNOUDT.
     MOVE     ENOUDT              TO   PARA-ENOUDT.
     MOVE     SCRTDT              TO   PARA-SSAKUSEI.
     MOVE     ECRTDT              TO   PARA-ESAKUSEI.
     MOVE     TORICD              TO   PARA-TORICD.
     MOVE     TANCD               TO   PARA-TANCD.
     MOVE     DTKBN               TO   PARA-DTKBN.
*
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
     WRITE    FKZ00501.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SOKO
                                  EDIT-CURSOR OF SNOUDT
                                  EDIT-CURSOR OF ENOUDT
                                  EDIT-CURSOR OF SCRTDT
                                  EDIT-CURSOR OF ECRTDT
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF TANCD
                                  EDIT-CURSOR OF DTKBN.
     MOVE     "M"            TO   EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF SNOUDT
                                  EDIT-OPTION OF ENOUDT
                                  EDIT-OPTION OF SCRTDT
                                  EDIT-OPTION OF ECRTDT
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF TANCD
                                  EDIT-OPTION OF DTKBN.
 CLR-HEAD-EXIT.
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
