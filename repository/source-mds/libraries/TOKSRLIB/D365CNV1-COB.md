# D365CNV1

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/D365CNV1.COB`

## ソースコード

```cobol
****************************************************************
*  顧客名　　　　：　サカタのタネ（株）殿　　　　　　　
*  業務名　　　　：　販売管理システム　　　　　　　　　
*  モジュール名　：　売上伝票ファイル仮伝分割キー採番　
*  　　　　　　　　　（コンバート）
*  作成日／更新日：　2021/05/27
*  作成者／更新者：　INOUE
*  処理概要　　　：　売上データ未作成のレコードに
*                  　仮伝分割キー/小売連携対象区分をセットする
*  更新履歴      ：
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            D365CNV1.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/05/27.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01  DEN-F02
                                       DEN-F04  DEN-F051
                                       DEN-F07  DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< SUB商品変換ＴＢＬ >>--*
     SELECT   SUBTBLF   ASSIGN         DA-01-VI-SUBTBLL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SUB-F01  SUB-F02
                        STATUS         SUBTBLF-ST1.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
**----<< SUB商品変換ＴＢＬ >>--*
 FD  SUBTBLF            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  ONL-FLG        PIC  9(01).
     03  TAN-ERR-FLG    PIC  9(01).
     03  TEI-FLG        PIC  9(01).
     03  SUB-INV-FLG    PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  OUT-CNT        PIC  9(07).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT            PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
 01  TANTO-WORK.
     03  TAN-OLD        PIC  9(02).
     03  TAN-NEW        PIC  9(02).
 01  WK-TANCD           PIC  9(02).
 01  WK-DEN-F45         PIC  9(08).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  SUBTBLF-ST.
     03  SUBTBLF-ST1    PIC  X(02).
     03  SUBTBLF-ST2    PIC  X(04).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-NOUHIN          PIC  9(08).
 01  FILLER             REDEFINES      WK-NOUHIN.
     03  WK-YYYY        PIC  9(02).
     03  WK-YMD         PIC  9(06).
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
 01  WK-TOKCD           PIC  9(08).
 01  CYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      CYU-DATE.
     03  CYU-YY         PIC  9(04).
     03  CYU-MM         PIC  9(02).
     03  CYU-DD         PIC  9(02).
 01  NOU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      NOU-DATE.
     03  NOU-YY         PIC  9(04).
     03  NOU-MM         PIC  9(02).
     03  NOU-DD         PIC  9(02).
 01  SYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      SYU-DATE.
     03  SYU-YY         PIC  9(04).
     03  SYU-MM         PIC  9(02).
     03  SYU-DD         PIC  9(02).
 01  CNV-YY             PIC  9(04)     VALUE  0.
 01  ACOS-MM            PIC  9(02)     VALUE  0.
 01  WK-BUMON           PIC  9(04)     VALUE  ZERO.
*----<< ﾌﾞﾚｲｸ ﾜｰｸ >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-F01    PIC  9(08).
         05  NEW-F02    PIC  9(09).
         05  NEW-F051   PIC  9(02).
     03  OLD.
         05  OLD-F01    PIC  9(08).
         05  OLD-F02    PIC  9(09).
         05  OLD-F051   PIC  9(02).
*
*----<< 仮伝分割キー ｴﾘｱ >>-*
 01  KARIDEN.
     03  KARIDEN-1    PIC  X(01) VALUE "S".
     03  KARIDEN-2    PIC  9(08) VALUE 20210601.
     03  KARIDEN-3    PIC  9(06) VALUE 000001.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
*--<< ｻﾌﾞﾙｰﾁﾝ ﾊﾟﾗﾒﾀ >>-*
 01  SSKTDTCK-PARA.
     03  SSKTDTCK-YMD        PIC  9(06).
     03  FILLER              REDEFINES SSKTDTCK-YMD.
         05  SSKTDTCK-Y      PIC  9(02).
         05  SSKTDTCK-M      PIC  9(02).
         05  SSKTDTCK-D      PIC  9(02).
     03  SSKTDTCK-RET        PIC  9(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*LINKAGE                   SECTION.
*01  PARA-JKDATE           PIC 9(08).
*01  PARA-JKTIME           PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### D365CNV1 SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< SUB商品変換ＴＢＬ >>--*
 SUB-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SUBTBLF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### D365CNV1 SUBTBLF ERROR " SUBTBLF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO     =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-TIME       TO   SYS-TIME2.
     DISPLAY  "*** D365CNV1 START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
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
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     SUBTBLF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     0         TO   GR-NO.
*----<< ｼｽﾃﾑﾋﾂﾞｹﾍﾝｶﾝ >>-*
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-YYMD.
*
 100-INIT-01.
*----<< 売上伝票ファイル　順ＲＥＡＤ　 >>-*
     PERFORM  900-DEN-READ.
     IF       NEW       =     HIGH-VALUE
              MOVE      99    TO   GR-NO
              GO        TO    100-INIT-RTN-EXIT
     END-IF.
*
 100-INIT-02.
*----<< ブレイクキー保管 >>-*
     MOVE     DEN-F01        TO   OLD-F01.
     MOVE     DEN-F02        TO   OLD-F02.
     MOVE     DEN-F051       TO   OLD-F051.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
 200-MAIN-01.
*----<< ブレイク判定 >>-*
     MOVE     DEN-F01          TO   NEW-F01.
     MOVE     DEN-F02          TO   NEW-F02.
     MOVE     DEN-F051         TO   NEW-F051.
     IF       NEW    NOT =     OLD
              MOVE   DEN-F01   TO   OLD-F01
              MOVE   DEN-F02   TO   OLD-F02
              MOVE   DEN-F051  TO   OLD-F051
              ADD    1         TO   KARIDEN-3
     END-IF.
*
 200-MAIN-02.
*----<< レコード更新 >>-*
     MOVE     DEN-F01          TO   SUB-F01.
     MOVE     DEN-F25          TO   SUB-F02.
     READ     SUBTBLF
              INVALID
                 MOVE  " "     TO   SUB-F19
     END-READ.
     IF       SUB-F19   NOT =  " "
              MOVE  SUB-F19    TO   DEN-F32
     END-IF.
     MOVE     KARIDEN          TO   DEN-D99.
*
     REWRITE  DEN-REC.
*
     ADD      1                TO   OUT-CNT.
*
 200-MAIN-03.
*----<< 売上伝票ファイル　順ＲＥＡＤ　 >>-*
     PERFORM  900-DEN-READ.
     IF       NEW       =     HIGH-VALUE
              MOVE      99    TO   GR-NO
              GO        TO    200-MAIN-RTN-EXIT
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    SHTDENF.
     CLOSE    SUBTBLF.
*
     DISPLAY  "+++ D365CNV1 ﾖﾐｺﾐ  ｹﾝｽｳ = " IN-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ D365CNV1 ｺｳｼﾝ  ｹﾝｽｳ = " OUT-CNT " +++"
                                       UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** D365CNV1 END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル　 READ                      *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     SHTDENF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
     ADD       1     TO   IN-CNT.
*
 DEN010.
     IF       DEN-F277 =  9
              GO   TO   900-DEN-READ
     END-IF.
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
