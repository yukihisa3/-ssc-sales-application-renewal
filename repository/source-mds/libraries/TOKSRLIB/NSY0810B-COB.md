# NSY0810B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0810B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　出荷業務（ＤＣＭ専用）　　　　　　*
*    モジュール名　　　　：　出荷明細データ抽出処理            *
*    作成日／更新日　　　：　2021/02/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*    作成日／更新日　　　：　2021/04/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注単位区分＝Ｃの時ケースへ変換  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NSY0810B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/09/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*出荷明細ワークファイル
     SELECT   DCMSMIF  ASSIGN    TO        DA-01-S-DCMSMIF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYU-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*ＤＣＭ基本情報ファイル
     SELECT   DNJOHOF   ASSIGN    TO        DA-01-VI-DNJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K05   DJS-K06
                                            DJS-K07   DJS-K08
                        FILE      STATUS    DJS-STATUS.
*発注種別変換マスタ
     SELECT   DCMHSBF   ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   HSB-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     DCMDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷明細ワークファイル
******************************************************************
 FD  DCMSMIF           BLOCK     CONTAINS  1    RECORDS.
     COPY     DCMSMIF  OF        XFDLIB
              JOINING   SYU       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    ＤＣＭ基本情報ファイル
******************************************************************
 FD  DNJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DNJOHOF   OF        XFDLIB
              JOINING   DJS       PREFIX.
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBF.
     COPY     DCMHSBL1  OF        XFDLIB
              JOINING   HSB       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  DNJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DCMHSBF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SYU-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  DJS-STATUS        PIC  X(02).
     03  HSB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NSY0810B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0810B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0810B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUTNO             PIC   9(01).
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUHIN            PIC   9(08).
 01  PARA-STEN              PIC   9(05).
 01  PARA-ETEN              PIC   9(05).
 01  PARA-SBUMON            PIC   X(04).
 01  PARA-EBUMON            PIC   X(04).
 01  PARA-STANA             PIC   X(06).
 01  PARA-ETANA             PIC   X(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUTNO
                                       PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUHIN
                                       PARA-STEN
                                       PARA-ETEN
                                       PARA-SBUMON
                                       PARA-EBUMON
                                       PARA-STANA
                                       PARA-ETANA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMSMIF.
     MOVE      "DCMSMIF"    TO   AB-FILE.
     MOVE      SYU-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNJOHOF.
     MOVE      "DNJOHOL1"    TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMHSBF.
     MOVE      "DCMHSBL1"    TO   AB-FILE.
     MOVE      HSB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLA  SHOTBL1  DNJOHOF  DCMHSBF.
     OPEN     OUTPUT    DCMSMIF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*****DISPLAY "PARA-SBUMON = " PARA-SBUMON UPON CONS.
*****DISPLAY "PARA-EBUMON = " PARA-EBUMON UPON CONS.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TORICD    =    DEN-F01 )
              IF   PARA-SOKO      NOT =     SPACE
                   IF   PARA-SOKO      =    DEN-F48
                        CONTINUE
                   ELSE
                        GO   TO   MAIN-010
                   END-IF
              END-IF
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
 MAIN-900.
*    店舗コード
     IF     ( PARA-STEN      <=   DEN-F07   ) AND
            ( DEN-F07        <=   PARA-ETEN )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
 MAIN-901.
*    納品日
     IF       PARA-NOUHIN    NOT =     ZERO
         IF   PARA-NOUHIN    =    DEN-F112
              CONTINUE
         ELSE
              GO        TO        MAIN-010
         END-IF
     ELSE
              CONTINUE
     END-IF.
 MAIN-902.
*    部門コード
     IF     ( PARA-SBUMON    <=   DEN-F12     ) AND
            ( DEN-F12        <=   PARA-EBUMON )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
 MAIN-903.
*    _番
     IF     ( PARA-STANA     <=   DEN-F49     ) AND
            ( DEN-F49        <=   PARA-ETANA  )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*出荷明細ワーク出力
     MOVE     SPACE          TO   SYU-REC.
     INITIALIZE                   SYU-REC.
     MOVE     DEN-F46        TO   SYU-F011.
     MOVE     DEN-F47        TO   SYU-F012.
     MOVE     DEN-F01        TO   SYU-F013.
     MOVE     DEN-F48        TO   SYU-F02.
     MOVE     DEN-F07        TO   SYU-F03.
     MOVE     DEN-F12        TO   SYU-F04.
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              MOVE  SPACE    TO   SYU-F05
       NOT INVALID
              MOVE  TBL-F08  TO   SYU-F05
     END-READ.
     MOVE     DEN-F1411      TO   SYU-F061.
     MOVE     DEN-F1412      TO   SYU-F062.
     MOVE     DEN-F25        TO   SYU-F07.
     MOVE     DEN-F1421      TO   SYU-F081.
     MOVE     DEN-F1422      TO   SYU-F082.
     MOVE     DEN-F50        TO   SYU-F09.
     MOVE     DEN-F15        TO   SYU-F10.
     MOVE     DEN-F172       TO   SYU-F11.
     MOVE     DEN-F173       TO   SYU-F12.
     MOVE     SPACE          TO   SYU-F13.
     MOVE     SPACE          TO   SYU-F14.
     MOVE     DEN-F112       TO   SYU-F15.
     MOVE     DEN-F02        TO   SYU-F16.
     MOVE     DEN-F27D       TO   SYU-F17.
     MOVE     DEN-M03        TO   SYU-F19.
     MOVE     DEN-M16        TO   SYU-F20.
     MOVE     DEN-M15        TO   SYU-F21.
*#2021/04/08 NAV ST発注単位区分は”Ｃ”の時は形状区分を３セット
     IF       DEN-M15 = "C"
              MOVE   "3"     TO   SYU-F20
     END-IF.
*#2021/04/08 NAV ED
     MOVE     DEN-M09        TO   SYU-F22.
*名称取得
     PERFORM  DNJOHOF-READ-SEC.
     IF DNJOHOF-INV-FLG  =  "INV"
        MOVE  SPACE    TO   SYU-F23 SYU-F25 SYU-F26
     ELSE
        IF  SYU-F22  =  SPACE
            MOVE  DJS-F232   TO   SYU-F23
            MOVE  DJS-F21    TO   SYU-F22
        END-IF
        MOVE  DJS-F262 TO   SYU-F23
        MOVE  DJS-F162 TO   SYU-F25
        MOVE  DJS-M086 TO   SYU-F26
     END-IF.
*発注種別区分取得
     PERFORM  DCMHSBF-READ-SEC.
     IF  DCMHSBF-INV-FLG  =  "INV"
        MOVE  SPACE    TO   SYU-F24
     ELSE
        MOVE  HSB-F04  TO   SYU-F24
     END-IF.
     WRITE    SYU-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　新ＤＣＭ基本情報Ｆ読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 DNJOHOF-READ-SEC     SECTION.
*
     MOVE      DEN-F46           TO       DJS-K01.
     MOVE      DEN-F47           TO       DJS-K02.
     MOVE      DEN-F01           TO       DJS-K03.
     MOVE      DEN-F48           TO       DJS-K04.
     MOVE      DEN-F07           TO       DJS-K05.
     MOVE      DEN-F02           TO       DJS-K06.
     MOVE      DEN-F03           TO       DJS-K07.
     MOVE      DEN-F112          TO       DJS-K08.
     READ  DNJOHOF
           INVALID      MOVE  "INV"  TO   DNJOHOF-INV-FLG
           NOT INVALID  MOVE  SPACE  TO   DNJOHOF-INV-FLG
     END-READ.
*
 DNJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　ＤＣＭ発注種別マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 DCMHSBF-READ-SEC     SECTION.
*
     MOVE      DEN-F01           TO       HSB-F01.
     MOVE      DEN-M03           TO       HSB-F02.
     READ  DCMHSBF
           INVALID      MOVE  "INV"  TO   DCMHSBF-INV-FLG
           NOT INVALID  MOVE  SPACE  TO   DCMHSBF-INV-FLG
     END-READ.
*
 DCMHSBF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENLA  DCMSMIF  SHOTBL1  DNJOHOF  DCMHSBF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
