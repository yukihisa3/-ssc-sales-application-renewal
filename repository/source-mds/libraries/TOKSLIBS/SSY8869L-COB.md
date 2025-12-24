# SSY8869L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8869L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　オンライン　　　*
*    モジュール名　　　　：　出荷リスト出力（部門／計上／発単）*
*    作成日／更新日　　　：　14/08/29                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ＤＣＭ出荷確定データを読み、出荷　*
*                            リストを発行する。（ホーマック様）*
*    作成日／更新日　　　：　18/01/26                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　形状区分＝４　他→大カゴへ変更　　*
*    作成日／更新日　　　：　19/02/21                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　発注種別区分対応　　　　　　　　　*
*    作成日／更新日　　　：　19/03/18                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　発注種別変換マスタ対応　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY8869L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/08/20.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＤＣＭ出荷確定データ >>--*
     SELECT   DJSYUKF   ASSIGN         DA-01-VI-DJSYUKLC
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KMK-K01
                                       KMK-K02
                                       KMK-K03
                                       KMK-K04
                                       KMK-F09
                                       KMK-F05
                                       KMK-K08
                                       KMK-F21
                                       KMK-F264
                                       KMK-F14
                                       KMK-M091
                                       KMK-M09
                                       KMK-F03
                                       KMK-M03
                        STATUS         DJSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*#2019/03/11 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL1  ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   DCMHSBL1-ST.
*#2019/03/11 NAV ED
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＤＣＭ出荷確定データ >>--*
 FD  DJSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   KMK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*#2019/03/11 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL1           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/11 NAV ED
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(08)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG        PIC  X(03)   VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DJSYUKF-ST        PIC  X(02).
*#2019/03/18 NAV ST
 01  DCMHSBL1-ST       PIC  X(02).
*#2019/03/18 NAV ED
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
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*伝票番号変換
 01  WK-DENPYO          PIC  X(07).
 01  FILLER             REDEFINES      WK-DENPYO.
     03  WK-DENPYO-R    PIC  9(07).
*
 01  WK-SURYO           PIC  9(05)V9   VALUE  ZERO.
 01  WK-SYUKA           PIC  9(05)V9   VALUE  ZERO.
 01  CNT-AFTER          PIC  9(02)     VALUE  ZERO.
 01  CHK-FLG            PIC  X(03)     VALUE  SPACE.
*#2019/03/18 NAV ST
 01  DCMHSBL1-INV-FLG   PIC  X(03)     VALUE  SPACE.
*#2019/03/18 NAV ED
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(09)V99.
     03  WK-GK-BAIKA    PIC  9(09).
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-KMK-K01     PIC  9(08).
     03  WK-KMK-K02     PIC  9(04).
     03  WK-KMK-K03     PIC  9(08).
     03  WK-KMK-K04     PIC  X(02).
     03  WK-KMK-F09     PIC  9(02).
     03  WK-KMK-F05     PIC  9(08).
     03  WK-KMK-K08     PIC  9(08).
     03  WK-KMK-F21     PIC  9(04).
     03  WK-KMK-F264    PIC  X(02).
     03  WK-KMK-F14     PIC  9(03).
     03  WK-KMK-M091    PIC  X(01).
     03  WK-KMK-M09     PIC  X(01).
     03  WK-KMK-F03     PIC  X(07).
     03  WK-KMK-F03-1   PIC  X(07).
     03  WK-KMK-M03     PIC  9(03).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  TL00.
     05  FILLER         PIC  X(01)     VALUE  SPACE.
     05  TL00-PAGE      PIC  ZZ9.
     05  TL00-KUGIR     PIC  X(01).
     05  TL00-SPAGE     PIC  ZZZ.
     05  FILLER         PIC  X(05)     VALUE  "ﾍﾟｰｼﾞ".
     05  FILLER         PIC  X(105)    VALUE  SPACE.
     05  TL00-DATE      PIC  X(10).
     05  FILLER         PIC  X(02)     VALUE  SPACE.
     05  TL00-TIME      PIC  X(05).
 01  HD001.
     03  FILLER         PIC  X(102)    VALUE  SPACE.
     03  FILLER         PIC  X(33)     VALUE
        "---------------------------------".
 01  HD002.
     03  FILLER         PIC  X(102)    VALUE  SPACE.
     03  FILLER         PIC  X(33)     VALUE
        "!               !               !".
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(09)     VALUE
             NC"ホーマック株式会社".
         05  FILLER     PIC  X(34)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-2-5.
         05  FILLER     PIC  N(05)     VALUE
             NC"出荷リスト".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(34)     VALUE  SPACE.
         05  FILLER     PIC  X(04)     VALUE  "!   ".
         05  FILLER     PIC  N(06)     VALUE
             NC"検品／検収者".
         05  FILLER     PIC  X(09)     VALUE  "   !     ".
         05  FILLER     PIC  N(04)     VALUE
             NC"検収印　".
         05  FILLER     PIC  X(05)     VALUE  "    !".
 01  HD011.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(40)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD011-1    PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD011-2    PIC  N(10).
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"取引先名称".
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"店".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"館名".
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"形状区分".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"発注単位区分".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TOKCD PIC  999999.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  HD03-JISYA PIC  X(20).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TENCD PIC  9999.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  HD03-TENNM PIC  X(20).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-KANNM PIC  X(20).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-HBUM  PIC  999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-KEIJY PIC  N(03).
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  HD03-HACTN PIC  N(03).
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  HD03-HACD  PIC  X(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-NOUD  PIC  X(10).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品番号".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
         05  FILLER     PIC  X(93)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(06)     VALUE  NC"発注伝票番号".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"品名".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"規格".
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"特売".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"申込票_".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷数".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"引合".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"売価".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(04)     VALUE  NC"　行番号".
 01  SEN                         CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
*
 01  MS01.
     03  FILLER                  CHARACTER  TYPE  MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  999999999.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-BUMON PIC  999.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備考：".
         05  FILLER     PIC  X(92)     VALUE  SPACE.
         05  MS01-DENNO2 PIC 999999999.
 01  MS02.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-GYO   PIC  999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYOCD PIC  X(08).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS02-JANCD PIC  X(13).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYONM1 PIC X(20).
         05  MS02-SYONM2 PIC X(20).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS02-TOK   PIC  X(09).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-MOS   PIC  X(09).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SURYO PIC  ---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYUKA PIC  ---,--9.
         05  FILLER     PIC  X(07)     VALUE  " (   ) ".
         05  MS02-BAITAN PIC  ---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-BAIKIN PIC  -,---,--9.
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  MS02-GYO2   PIC  999.
 01  MS03.
     03  FILLER.
         05  FILLER     PIC  X(120)    VALUE  SPACE.
         05  MS03-BAIGOK PIC  --,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "-".
*
 01  MSG-AREA.
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
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
*ADD NAV OONO 2010/02/23 BGN
 01  LINK-SDENCD            PIC  9(09).
 01  LINK-EDENCD            PIC  9(09).
*ADD NAV OONO 2010/02/23 END
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT
*ADD NAV OONO 2010/02/23 BGN
                                         LINK-SDENCD
                                         LINK-EDENCD.
*ADD NAV OONO 2010/02/23 END
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カーマ出荷確定データ >>--*
 DJSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DJSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8869L DJSYUKF ERROR " DJSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     CLOSE    DJSYUKF  DCMHSBL1.
     STOP     RUN.
*#2019/03/18 NAV ST
*----<< 発注種別変換マスタ >>--*
 DCMHSBF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMHSBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8869L DCMHSBL1 ERROR " DCMHSBL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     CLOSE    DJSYUKF  DCMHSBL1.
     STOP     RUN.
*#2019/03/18 NAV ED
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     DISPLAY  "*** SSY8869L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DJSYUKF.
     OPEN     OUTPUT    PRTF.
*#2019/03/18 NAV ST
     OPEN     INPUT     DCMHSBL1.
*#2019/03/18 NAV ED
*
     INITIALIZE  BREAK-KEY.
*カーマ出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   KMK-REC.
     INITIALIZE                        KMK-REC.
     MOVE     LINK-JDATE          TO   KMK-K01.
     MOVE     LINK-JTIME          TO   KMK-K02.
     MOVE     LINK-TORICD         TO   KMK-K03.
     MOVE     LINK-SOKO           TO   KMK-K04.
     MOVE     LINK-STENCD         TO   KMK-K05.
     DISPLAY  "LINK-JDATE = " LINK-JDATE UPON CONS.
     DISPLAY  "LINK-JTIME = " LINK-JTIME UPON CONS.
     DISPLAY  "LINK-TORICD= " LINK-TORICD UPON CONS.
     DISPLAY  "LINK-SOKO  = " LINK-SOKO  UPON CONS.
     DISPLAY  "LINK-STENCD= " LINK-STENCD UPON CONS.
*ADD NAV OONO 2010/02/23 BGN
     DISPLAY  "LINK-SDENCD= " LINK-SDENCD UPON CONS.
*ADD NAV OONO 2010/02/23 END
     PERFORM  900-KMK-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     KMK-K01    TO   WK-KMK-K01
              MOVE     KMK-K02    TO   WK-KMK-K02
              MOVE     KMK-K03    TO   WK-KMK-K03
              MOVE     KMK-K04    TO   WK-KMK-K04
              MOVE     KMK-F09    TO   WK-KMK-F09
              MOVE     KMK-F05    TO   WK-KMK-F05
              MOVE     KMK-K08    TO   WK-KMK-K08
              MOVE     KMK-F21    TO   WK-KMK-F21
              MOVE     KMK-F264   TO   WK-KMK-F264
              MOVE     KMK-M091   TO   WK-KMK-M091
              MOVE     KMK-M09    TO   WK-KMK-M09
              MOVE     KMK-F14    TO   WK-KMK-F14
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT  PAGE-CNT2
              MOVE     SPACE      TO   CHK-FLG
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    発注伝票区分／発注日／出荷日／店ＣＤ／館ＣＤブレイク時
*    改ページ
     IF       KMK-F09   NOT =  WK-KMK-F09
     OR       KMK-F05   NOT =  WK-KMK-F05
     OR       KMK-K08   NOT =  WK-KMK-K08
     OR       KMK-F21   NOT =  WK-KMK-F21
     OR       KMK-F264  NOT =  WK-KMK-F264
     OR       KMK-M091  NOT =  WK-KMK-M091
     OR       KMK-M09   NOT =  WK-KMK-M09
     OR       KMK-F14   NOT =  WK-KMK-F14
              PERFORM   GOKEI-WT-SEC
              MOVE      "CHK"     TO  CHK-FLG
              PERFORM   HEAD-WT-SEC
              PERFORM   MEISAI-HEAD-SEC
              MOVE      KMK-F09   TO  WK-KMK-F09
              MOVE      KMK-F05   TO  WK-KMK-F05
              MOVE      KMK-K08   TO  WK-KMK-K08
              MOVE      KMK-F21   TO  WK-KMK-F21
              MOVE      KMK-F264  TO  WK-KMK-F264
              MOVE      KMK-F03(1:7)   TO  WK-KMK-F03
              MOVE      KMK-M091  TO  WK-KMK-M091
              MOVE      KMK-M09   TO  WK-KMK-M09
              MOVE      KMK-F14   TO  WK-KMK-F14
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
              MOVE      SPACE     TO  CHK-FLG
     END-IF.
*    伝票番号がブレイク
     MOVE     KMK-F03(1:7)        TO  WK-KMK-F03-1.
     IF       WK-KMK-F03-1   NOT =  WK-KMK-F03
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
**************MOVE      KMK-F03   TO  WK-KMK-F03
              MOVE      KMK-F03(1:7)   TO  WK-KMK-F03
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    カーマ出荷確定データ読込み
     PERFORM  900-KMK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
         PERFORM  TAIL-WT-SEC
     END-IF.
*
     CLOSE    DJSYUKF  PRTF.
*#2019/03/18 NAV ST
     CLOSE    DCMHSBL1.
*#2019/03/18 NAV ED
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8869L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              PERFORM  TAIL-WT-SEC
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
              IF  CHK-FLG = "CHK"
                  MOVE      ZERO      TO   PAGE-CNT2
              END-IF
     END-IF.
*    行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*    頁カウンター
     ADD      1         TO        PAGE-CNT  PAGE-CNT2.
*    発注種別区分
     MOVE     KMK-F09        TO   HD011-1.
     EVALUATE KMK-F09
         WHEN 0
              MOVE NC"　定　期　　　　　　" TO HD011-2
         WHEN 1
              MOVE NC"　特　売　　　　　　" TO HD011-2
         WHEN 2
              MOVE NC"　新店改装　　　　　" TO HD011-2
         WHEN 3
              MOVE NC"　投　入　　　　　　" TO HD011-2
         WHEN 4
              MOVE NC"　ダイレクト　　　　" TO HD011-2
         WHEN 5
              MOVE NC"　客注ダイレクト　　" TO HD011-2
         WHEN 6
              MOVE NC"　新規初回　　　　　" TO HD011-2
         WHEN 51
              MOVE NC"　本部在庫補充　　　" TO HD011-2
         WHEN 52
              MOVE NC"　専用_在庫補充　　" TO HD011-2
         WHEN 71
              MOVE NC"　用度品　　　　　　" TO HD011-2
*#2019/02/21 NAV ST
         WHEN 82
              MOVE NC"　改廃　　　　　　　" TO HD011-2
         WHEN 83
              MOVE NC"　ＢＹ改廃　　　　　" TO HD011-2
*#2019/02/21 NAV ED
     END-EVALUATE.
*#2019/03/18 NAV ST 発注種別変換マスタより取得
     MOVE     KMK-K03                      TO   HSB-F01.
     MOVE     KMK-F09                      TO   HSB-F02.
     PERFORM  DCMHSBL1-READ-SEC.
     IF  DCMHSBL1-INV-FLG  =  SPACE
              MOVE   HSB-F09               TO   HD011-2
     ELSE
              MOVE   NC"発注種別未登録"    TO   HD011-2
     END-IF.
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*    ベンダーＣＤ
     MOVE     KMK-F304       TO   HD03-TOKCD.
*    ベンダー名称
     MOVE     KMK-F29        TO   HD03-JISYA.
*    店ＣＤ
     MOVE     KMK-F21        TO   HD03-TENCD.
*    店舗名称
     MOVE     KMK-F22        TO   HD03-TENNM.
*    館名称
     EVALUATE KMK-F264
         WHEN "01"
              MOVE "ﾎｰﾑｾﾝﾀｰ"        TO  HD03-KANNM
         WHEN "02"
              MOVE "ｼｻﾞｲｾﾝﾀｰ"       TO  HD03-KANNM
         WHEN "03"
              MOVE "ﾎｰﾑｲﾝﾃﾘｱｾﾝﾀｰ"   TO  HD03-KANNM
         WHEN "04"
              MOVE "ｶﾞｰﾃﾞﾝｾﾝﾀｰ"     TO  HD03-KANNM
         WHEN "05"
              MOVE "ﾍﾟｯﾄ&ｶﾞｰﾃﾞﾝ"    TO  HD03-KANNM
         WHEN "06"
              MOVE "ｼｻﾞｲ&ｶﾞｰﾃﾞﾝ"    TO  HD03-KANNM
         WHEN "07"
              MOVE "ﾍﾟｯﾄ"           TO  HD03-KANNM
     END-EVALUATE.
*    部門
     MOVE     KMK-F14               TO  HD03-HBUM.
*    形状区分
     EVALUATE KMK-M091
         WHEN "1"
              MOVE NC"小物　"       TO  HD03-KEIJY
         WHEN "2"
              MOVE NC"異形　"       TO  HD03-KEIJY
         WHEN "3"
              MOVE NC"ケース"       TO  HD03-KEIJY
*#2018/01/26 NAV ST
         WHEN "4"
              MOVE NC"大カゴ"       TO  HD03-KEIJY
*#2018/01/26 NAV ED
         WHEN OTHER
              MOVE NC"＊＊＊"       TO  HD03-KEIJY
     END-EVALUATE.
*    発注単位区分名
     EVALUATE KMK-M09
         WHEN "P"
              MOVE NC"ピース"       TO  HD03-HACTN
         WHEN "C"
              MOVE NC"ケース"       TO  HD03-HACTN
         WHEN OTHER
              MOVE NC"＊＊＊"       TO  HD03-HACTN
     END-EVALUATE.
*    発注日
     MOVE     KMK-F05(1:4)   TO   WK-H-YYYY.
     MOVE     KMK-F05(5:2)   TO   WK-H-MM.
     MOVE     KMK-F05(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-HACD.
*    出荷日
     MOVE     KMK-K08(1:4)   TO   WK-H-YYYY.
     MOVE     KMK-K08(5:2)   TO   WK-H-MM.
     MOVE     KMK-K08(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-NOUD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD011 AFTER  0.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  2.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  1.
*行カウント
     MOVE     13             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ヘッダー印字
*--------------------------------------------------------------*
 MEISAI-HEAD-SEC       SECTION.
     MOVE     "MEISAI-HEAD-SEC"    TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  52
            PERFORM   HEAD-WT-SEC
     END-IF.
*    納品番号（伝票番号）
*****MOVE      KMK-K06     TO   MS01-DENNO.
     MOVE      KMK-F03     TO   WK-DENPYO.
     MOVE      WK-DENPYO-R TO   MS01-DENNO.
*    部門
     MOVE      KMK-F14     TO   MS01-BUMON.
*    発注伝票番号
*****MOVE      KMK-K06     TO   MS01-DENNO2.
     MOVE      KMK-F03     TO   WK-DENPYO.
     MOVE      WK-DENPYO-R TO   MS01-DENNO2.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
*
     ADD       1           TO   LINE-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  52
            PERFORM   HEAD-WT-SEC
     END-IF.
*    行
     MOVE   KMK-M03               TO   MS02-GYO.
*    商品ＣＤ
     MOVE   KMK-M04               TO   MS02-SYOCD.
*    ＪＡＮＣＤ
     MOVE   KMK-M02               TO   MS02-JANCD.
*    品名
     MOVE   KMK-M07(1:20)         TO   MS02-SYONM1.
*    規格
     MOVE   KMK-M07(21:15)        TO   MS02-SYONM2.
*    特売ＣＤ
     MOVE   KMK-F061              TO   MS02-TOK.
*    申込票
     MOVE   KMK-F311              TO   MS02-MOS.
*    発注数
     IF     KMK-M11  >  ZERO
            COMPUTE  MS02-SURYO  =  KMK-M11  /  100
     ELSE
            MOVE     ZERO         TO   MS02-SURYO
     END-IF.
*    出荷数
     MOVE   KMK-A36               TO   MS02-SYUKA WK-SURYO.
*    引合
*    売価
     MOVE   KMK-M17               TO   MS02-BAITAN.
*    売価金額
     COMPUTE  MS02-BAIKIN  =  ( WK-SURYO * KMK-M17  ).
     COMPUTE  WK-GK-BAIKA  =  WK-GK-BAIKA +
                              ( WK-SURYO * KMK-M17  ).
*    発注伝票行番号
     MOVE     KMK-M03         TO    MS02-GYO2.
*    〇以上の場合に出力対象
     WRITE     PRT-REC   FROM  MS02  AFTER  1.
     ADD       1           TO   LINE-CNT.
*----< 2007/06/07 修正終了 >----
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC" TO   S-NAME.
*    売価金額合計
     MOVE     WK-GK-BAIKA       TO    MS03-BAIGOK.
*
     WRITE     PRT-REC   FROM  MS03  AFTER  1.
     WRITE     PRT-REC   FROM  SEN1  AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       2           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-KMK-START-READ     SECTION.
     MOVE     "900-KMK-START-READ"     TO   S-NAME.
     START    DJSYUKF   KEY  >=   KMK-K01  KMK-K02
                                  KMK-K03  KMK-K04
                                  KMK-F09  KMK-F05
                                  KMK-K08  KMK-F21
                                  KMK-F264 KMK-F14  KMK-M091
                                  KMK-M09  KMK-F03  KMK-M03
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-KMK-START-READ-EXIT
     END-START.
*
     PERFORM   900-KMK-READ.
*
 900-KMK-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    カーマ出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-KMK-READ           SECTION.
     MOVE     "900-KMK-READ"      TO   S-NAME.
*
     READ     DJSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-KMK-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
          IF       KMK-K01   NOT =  LINK-JDATE       OR
                   KMK-K02   NOT =  LINK-JTIME       OR
                   KMK-K03   NOT =  LINK-TORICD
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-KMK-READ-EXIT
          END-IF
     ELSE
          IF       KMK-K01   NOT =  LINK-JDATE       OR
                   KMK-K02   NOT =  LINK-JTIME       OR
                   KMK-K03   NOT =  LINK-TORICD      OR
                   KMK-K04   NOT =  LINK-SOKO
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-KMK-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  KMK-K05
     AND      LINK-ETENCD  >=  KMK-K05
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-KMK-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  KMK-K08
     AND      LINK-ENOUDT  >=  KMK-K08
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-KMK-READ
     END-IF.
*
*伝票番号範囲チェック
     IF       LINK-SDENCD  <=  KMK-K06
     AND      LINK-EDENCD  >=  KMK-K06
              CONTINUE
     ELSE
              GO        TO        900-KMK-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-KMK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    テイル出力処理　　　　　 READ                *
*--------------------------------------------------------------*
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT-SEC"       TO   S-NAME.
*改行数算出
     COMPUTE  CNT-AFTER  =  54  -  LINE-CNT.
*テイル行編集
*ページ数セット
     MOVE     PAGE-CNT2     TO   TL00-PAGE.
*総ページ数セット
     MOVE     ZERO          TO   TL00-SPAGE.
     MOVE     SPACE         TO   TL00-KUGIR.
*システム日付
     MOVE     SYS-YYW       TO   TL00-DATE(1:4).
     MOVE     "/"           TO   TL00-DATE(5:1).
     MOVE     SYS-MMW       TO   TL00-DATE(6:2).
     MOVE     "/"           TO   TL00-DATE(8:1).
     MOVE     SYS-DDW       TO   TL00-DATE(9:2).
*システム時刻
     MOVE     SYS-HH        TO   TL00-TIME(1:2).
     MOVE     ":"           TO   TL00-TIME(3:1).
     MOVE     SYS-MN        TO   TL00-TIME(4:2).
*
     IF  CNT-AFTER > 1
         WRITE     PRT-REC   FROM  SEN1  AFTER  CNT-AFTER
     END-IF.
     WRITE     PRT-REC   FROM  TL00  AFTER  1.
*
 TAIL-WT-EXIT.
     EXIT.
*#2019/03/18 NAV ST
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL1-READ-SEC         SECTION.
*
     READ     DCMHSBL1
         INVALID
           MOVE  "INV"     TO        DCMHSBL1-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL1-INV-FLG
     END-READ.
*
 DCMHSBL1-READ-EXIT.
     EXIT.
*#2019/03/18 NAV ED
*-----------------<< PROGRAM END >>----------------------------*

```
