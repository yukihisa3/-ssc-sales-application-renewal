# PSO0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/PSO0010I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    業務名　　　　　　　：　ダイユーエイト新ＥＤＩシステム    *
*    モジュール名　　　　：　ＰＯＳ売上情報集計リスト発行　　  *
*    作成日／更新日　　　：　2010/07/15                        *
*    作成者／更新者　　　：　大野                              *
*    処理概要　　　　　　：　画面より、取引先ＣＤ・売上期間・出*
*                            力店舗・商品ＣＤを入力し、パラメタ*
*                            へ渡す。                          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            PSO0010I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/15.
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
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   SHOTBL1-ST.
*                       FILE STATUS    IS   TBL-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
*                       STATUS         TEN-STATUS.
                        STATUS         HTENMS-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< POS売上情報ファイル >>--*
     SELECT   DYPSURL2  ASSIGN         DA-01-VI-DYPSURL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DYP-F01   DYP-F02
                                       DYP-F04   DYP-F05
                        STATUS         DYPSURL2-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FPO0010  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     SHOTBL1   OF        XFDLIB
              JOINING   TBL       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< POS売上情報F >>--*
 FD  DYPSURL2           LABEL RECORD   IS   STANDARD.
     COPY     DYPSURL2  OF        XFDLIB
              JOINING   DYP       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
*01  DYJOHOF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  SHOTBL1-ST        PIC  X(02).
 01  DYPSURL2-ST       PIC  X(02).
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
 01  WK-TORICD          PIC  X(02)  VALUE  SPACE.
 01  WK-TORINM          PIC  N(15)  VALUE  SPACE.
 01  WK-SURYMD          PIC  9(08)  VALUE  ZERO.
 01  WK-EURYMD          PIC  9(08)  VALUE  ZERO.
 01  WK-STENCD          PIC  9(08)  VALUE  ZERO.
 01  WK-ETENCD          PIC  9(08)  VALUE  ZERO.
 01  WK-SSHOCD          PIC  9(08)  VALUE  ZERO.
 01  WK-ESHOCD          PIC  9(08)  VALUE  ZERO.
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
              NC"取引先コードを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"取引先コードが存在しません。".
     03  MSG04               PIC  N(20)  VALUE
              NC"正しい日付を入力して下さい。".
     03  MSG05               PIC  N(20)  VALUE
              NC"売上期間の指定が間違っています。".
     03  MSG06               PIC  N(20)  VALUE
              NC"店舗マスタに存在しません。".
     03  MSG07               PIC  N(20)  VALUE
              NC"出力店舗の指定が間違っています。".
     03  MSG08               PIC  N(20)  VALUE
              NC"商品ＣＤがマスタに登録されていません。".
     03  MSG09               PIC  N(20)  VALUE
              NC"商品ＣＤの指定が間違っています。".
     03  MSG10               PIC  N(20)  VALUE
              NC"該当するデータはありません。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      10.
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
   01  PARA-TORICD        PIC   9(08).
   01  PARA-SURYMD        PIC   9(08).
   01  PARA-EURYMD        PIC   9(08).
   01  PARA-STENCD        PIC   9(06).
   01  PARA-ETENCD        PIC   9(06).
   01  PARA-SSHOCD        PIC   9(13).
   01  PARA-ESHOCD        PIC   9(13).
   01  PARA-CHOSHU        PIC   9(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD
                                       PARA-SURYMD
                                       PARA-EURYMD
                                       PARA-STENCD
                                       PARA-ETENCD
                                       PARA-SSHOCD
                                       PARA-ESHOCD
                                       PARA-CHOSHU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### PSO0010I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS  HTENMS  SHOTBL1  DYPSURL2  DSPFILE.
     STOP     RUN.
*
*----<< 基本情報ファイル >>--*
 SHOTBL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### PSO0010I SHOTBL1 ERROR " SHOTBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS  HTENMS  SHOTBL1  DYPSURL2  DSPFILE.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### PSO0010I HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS  HTENMS  SHOTBL1  DYPSURL2  DSPFILE.
     STOP     RUN.
*
*----<< POS売上情報F >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DYPSURL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### PSO0010I DYPSURL2 ERROR " DYPSURL2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HTOKMS  HTENMS  SHOTBL1  DYPSURL2  DSPFILE.
     STOP     RUN.
*
*
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
*ﾁｪｯｸ取引先CDｾｯﾄ
*    MOVE     883                 TO   WK-HCSZHOK.
*    MOVE     882                 TO   WK-HCSZTOH.
*    MOVE     880                 TO   WK-HCSZKAN.
*    MOVE     13938               TO   WK-KAHMASZ.
*    MOVE     14273               TO   WK-HCSKHOK.
*    MOVE     14272               TO   WK-HCSKTOH.
*    MOVE     1427                TO   WK-HCSKKAN.
*    MOVE     17137               TO   WK-KAHMASK.
*    MOVE     100403              TO   WK-DAIK1.
*    MOVE     1004031             TO   WK-DAIK2.

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
     DISPLAY  "*** PSO0010I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     DYPSURL2.
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
*----<< 取引先コードなど入力>>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
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
     CLOSE    HTOKMS  HTENMS  SHOTBL1  DYPSURL2  DSPFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** PSO0010I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"  TO   S-NAME.
     MOVE     SPACE          TO   FPO0010.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "PSO0010I"      TO   PGID.
     MOVE    "FPO0010"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FPO0010"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      取引先コードなど入力                        *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
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
*    LEVEL  3      取引先コードなど入力チェック                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    取引先チェック
*    取引先コードが未入力の1合
     IF  TORICD  NOT  NUMERIC
     OR  TORICD  =  ZERO
         IF   ERR-FLG   =    ZERO
              MOVE      2    TO   ERR-FLG
         END-IF
         MOVE      ZERO TO   TORICD
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     ELSE
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
             NOT INVALID
                   MOVE      TOK-F02   TO   TORINM
         END-READ
     END-IF.
*    売上期間チェック
*    売上期間開始のチェック
     IF       SURYMD NOT NUMERIC
              IF   ERR-FLG   =    ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SURYMD
              MOVE     "R"   TO   EDIT-OPTION OF SURYMD
*             MOVE      ZERO      TO   SURYMD
     ELSE
              IF   SURYMD NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     SURYMD    TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
*                  IF       LINK-OUT-RET   =        ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE       4   TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF SURYMD
                            MOVE  "R"   TO  EDIT-OPTION OF SURYMD
                   END-IF
              END-IF
     END-IF.
*    売上期間終了のチェック
     IF       EURYMD NOT NUMERIC
              IF   ERR-FLG   =    ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF EURYMD
              MOVE     "R"   TO   EDIT-OPTION OF EURYMD
*             MOVE      ZERO      TO   EURYMD
     ELSE
*             IF   EURYMD NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     EURYMD    TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
*                  IF       LINK-OUT-RET   =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      4    TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF EURYMD
                            MOVE  "R"   TO  EDIT-OPTION OF EURYMD
                   END-IF
*             END-IF
     END-IF.
*
*    売上期限開始・終了の大小チェック
     IF  SURYMD  >  EURYMD
         IF   ERR-FLG   =    ZERO
              MOVE      5    TO   ERR-FLG
              MOVE  "C"   TO  EDIT-CURSOR OF SURYMD
         END-IF
*        MOVE  "C"   TO  EDIT-CURSOR OF SURYMD
         MOVE  "R"   TO  EDIT-OPTION OF SURYMD
         MOVE  "R"   TO  EDIT-OPTION OF EURYMD
     END-IF.
*    店舗コード開始チェック
     IF  STENCD  NOT  NUMERIC
     OR  STENCD  =  ZERO
*        未入力の場合
         MOVE      000001  TO   STENCD
     ELSE
         IF  STENCD    NOT =     1
             MOVE      SPACE     TO   TEN-REC
             INITIALIZE               TEN-REC
             MOVE      TORICD    TO   TEN-F52
             MOVE      STENCD    TO   TEN-F011
             READ      HTENMS
                 INVALID
*                      MOVE      SPACE     TO   TORINM
                       IF   ERR-FLG   =    ZERO
                            MOVE      6    TO   ERR-FLG
                       END-IF
                       MOVE     "C"   TO   EDIT-CURSOR OF STENCD
                       MOVE     "R"   TO   EDIT-OPTION OF STENCD
             END-READ
         END-IF
     END-IF.
*    店舗コード終了チェック
     IF  ETENCD  NOT  NUMERIC
     OR  ETENCD  =  ZERO
*        未入力の場合
         MOVE      999999  TO   ETENCD
     ELSE
         IF  ETENCD  NOT =   999999
             MOVE      SPACE     TO   TEN-REC
             INITIALIZE               TEN-REC
             MOVE      TORICD    TO   TEN-F52
             MOVE      ETENCD    TO   TEN-F011
             READ      HTENMS
                 INVALID
*                      MOVE      SPACE     TO   TORINM
                       IF   ERR-FLG   =    ZERO
                            MOVE      6    TO   ERR-FLG
                       END-IF
                       MOVE     "C"   TO   EDIT-CURSOR OF ETENCD
                       MOVE     "R"   TO   EDIT-OPTION OF ETENCD
             END-READ
         END-IF
     END-IF.
*
*    店舗コード開始・終了の大小チェック
     IF  STENCD  >  ETENCD
         IF   ERR-FLG   =    ZERO
              MOVE      7    TO   ERR-FLG
              MOVE  "C"   TO  EDIT-CURSOR OF STENCD
         END-IF
*        MOVE  "C"   TO  EDIT-CURSOR OF STENCD
         MOVE  "R"   TO  EDIT-OPTION OF STENCD
     END-IF.
*    商品コード開始チェック
     IF  SSHOCD  NOT  NUMERIC
     AND SSHOCD  =  ZERO
*        未入力の場合
         MOVE      0000000000001  TO   SSHOCD
     ELSE
         IF  SSHOCD  NOT  =      1
             MOVE      SPACE     TO   TBL-REC
             INITIALIZE               TBL-REC
             MOVE      TORICD    TO   TBL-F01
             MOVE      SSHOCD    TO   TBL-F02
             READ      SHOTBL1
                 INVALID
                       IF   ERR-FLG   =    ZERO
                            MOVE      8    TO   ERR-FLG
                       END-IF
                       MOVE     "C"   TO   EDIT-CURSOR OF SSHOCD
                       MOVE     "R"   TO   EDIT-OPTION OF SSHOCD
             END-READ
         END-IF
     END-IF.
*    商品コード終了チェック
     IF  ESHOCD  NOT  NUMERIC
     AND ESHOCD  =  ZERO
*        未入力の場合
         MOVE      9999999999999  TO   ESHOCD
     ELSE
         IF  ESHOCD  NOT  =  9999999999999
             MOVE      SPACE     TO   TBL-REC
             INITIALIZE               TBL-REC
             MOVE      TORICD    TO   TBL-F01
             MOVE      ESHOCD    TO   TBL-F02
             READ      SHOTBL1
                 INVALID
                       IF   ERR-FLG   =    ZERO
                            MOVE      8    TO   ERR-FLG
                       END-IF
                       MOVE     "C"   TO   EDIT-CURSOR OF ESHOCD
                       MOVE     "R"   TO   EDIT-OPTION OF ESHOCD
             END-READ
         END-IF
     END-IF.
*
*    商品コード開始・終了の大小チェック
     IF  SSHOCD  >  ESHOCD
         IF   ERR-FLG   =    ZERO
              MOVE      9    TO   ERR-FLG
              MOVE  "C"   TO  EDIT-CURSOR OF SSHOCD
         END-IF
         MOVE  "R"   TO  EDIT-OPTION OF SSHOCD
     END-IF.
*    POS売上情報ファイルチェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   DYP-REC
         INITIALIZE                    DYP-REC
         MOVE      TORICD         TO   DYP-F01
         MOVE      STENCD         TO   DYP-F02
         MOVE      SSHOCD         TO   DYP-F04
         MOVE      SURYMD         TO   DYP-F05
         START     DYPSURL2  KEY  >=   DYP-F01   DYP-F02
                                       DYP-F04   DYP-F05
              INVALID   KEY
                   MOVE 10    TO   ERR-FLG
                   MOVE "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE "R"   TO   EDIT-OPTION OF TORICD
**                 DISPLAY  " ERR1 " UPON CONS
              NOT INVALID
                   READ   DYPSURL2   NEXT
                     AT END
                        MOVE     10    TO   ERR-FLG
                        MOVE "C"   TO   EDIT-CURSOR OF TORICD
                        MOVE "R"   TO   EDIT-OPTION OF TORICD
**                      DISPLAY  " ERR2 " UPON CONS
                     NOT AT END
                      IF   DYP-F01  <  TORICD AND
                           DYP-F02  <  STENCD AND
                           DYP-F04  <  SSHOCD AND
                           DYP-F05  <  SURYMD
                          MOVE 10    TO   ERR-FLG
                          MOVE "C"   TO   EDIT-CURSOR OF TORICD
                          MOVE "R"   TO   EDIT-OPTION OF TORICD
*                         DISPLAY " TORICD " TORICD UPON CONS
*                         DISPLAY " STENCD " STENCD UPON CONS
*                         DISPLAY " SSHOCD " SSHOCD UPON CONS
*                         DISPLAY " SURYMD " SURYMD UPON CONS
*                         DISPLAY " DYP-F01 " DYP-F01 UPON CONS
*                         DISPLAY " DYP-F02 " DYP-F02 UPON CONS
*                         DISPLAY " DYP-F04 " DYP-F04 UPON CONS
*                         DISPLAY " DYP-F05 " DYP-F05 UPON CONS
*                         DISPLAY  " ERR3 " UPON CONS
                          GO  TO  220-GRP01-CHECK-EXIT
                      END-IF
                      IF   DYP-F01  >  TORICD OR
                           DYP-F02  >  ETENCD OR
                           DYP-F04  >  ESHOCD OR
                           DYP-F05  >  EURYMD
                          MOVE 10    TO   ERR-FLG
                          MOVE "C"   TO   EDIT-CURSOR OF TORICD
                          MOVE "R"   TO   EDIT-OPTION OF TORICD
*                         DISPLAY  " ERR4 " UPON CONS
                          GO  TO  220-GRP01-CHECK-EXIT
                      END-IF
                   END-READ
         END-START
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
     MOVE     TORICD              TO   PARA-TORICD.
     MOVE     SURYMD              TO   PARA-SURYMD.
     MOVE     EURYMD              TO   PARA-EURYMD.
     MOVE     STENCD              TO   PARA-STENCD.
     MOVE     ETENCD              TO   PARA-ETENCD.
     MOVE     SSHOCD              TO   PARA-SSHOCD.
     MOVE     ESHOCD              TO   PARA-ESHOCD.
     MOVE     "1"                 TO   PARA-CHOSHU.
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
     WRITE    FPO0010.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF SURYMD
                                  EDIT-CURSOR OF EURYMD
                                  EDIT-CURSOR OF STENCD
                                  EDIT-CURSOR OF ETENCD
                                  EDIT-CURSOR OF SSHOCD
                                  EDIT-CURSOR OF ESHOCD.
*
     MOVE     "M"            TO   EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SURYMD
                                  EDIT-OPTION OF EURYMD
                                  EDIT-OPTION OF STENCD
                                  EDIT-OPTION OF ETENCD
                                  EDIT-OPTION OF SSHOCD
                                  EDIT-OPTION OF ESHOCD.
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
