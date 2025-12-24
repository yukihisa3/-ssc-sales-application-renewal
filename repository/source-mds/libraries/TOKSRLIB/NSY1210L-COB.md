# NSY1210L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY1210L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭセンター納品対応　　　　　　*
*    モジュール名　　　　：　特注ラベル発行　　　　　　　　　　*
*    作成日／更新日　　　：　2021/02/25                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより、店舗ＣＤ、部門、個数*
*                        ：　を受取り、特注センターラベルを発行*
*                            する。                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NSY1210L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          21/02/25.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        FILE      STATUS    PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<<プリントファイル　　　　　　　>>----*
 FD  PRTFILE
                        LABEL  RECORD  IS   OMITTED
                        LINAGE IS      60   LINES
                        DATA   RECORD  IS   PRT-REC.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  ZERO-FLG       PIC  9(01)   VALUE ZERO.
 01  WK-CNT.
     03  IXA            PIC  9(03)   VALUE ZERO.
     03  IX             PIC  9(03)   VALUE ZERO.
     03  IZ             PIC  9(03)   VALUE ZERO.
     03  WK-KOSU        PIC  9(03)   VALUE ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  TEN-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  HTENMS-INV-FLG    PIC  X(03)      VALUE  SPACE.
 01  PG-ID             PIC  X(08)      VALUE  "NSY2110L".
*----<< ﾜｰｸ >>--*
 01  WK-AREA.
   03  WK-TENCD        PIC  9(05).
   03  WK-NOUHIN       PIC  9(08).
*    漢字変換
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
 01  WK-HENKAN-KANJI.
     03  WK-HENKAN-N.
         05  WK-HENKAN-1          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-2          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-3          PIC  N(01)  VALUE  SPACE.
 01  WK-HENKAN                    PIC  9(03).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  LBL1                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL1-A                  OCCURS     4.
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*******05  LBL1-01               PIC  N(10).
*******05  FILLER                PIC  X(12)  VALUE  SPACE.
       05  LBL1-01               PIC  N(15).
       05  FILLER                PIC  X(02)  VALUE  SPACE.
*
 01  LBL2                        CHARACTER  TYPE  IS  CHR-21.
     03  LBL2-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL2-01               PIC  N(08).
       05  FILLER                PIC  X(06)  VALUE  SPACE.
*
 01  LBL3                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL3-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL3-01               PIC  N(04).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL3-02               PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL3-03               PIC  N(01).
       05  LBL3-031              PIC  N(03).
       05  FILLER                PIC  X(10)  VALUE  SPACE.
*
 01  LBL4                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL4-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL4-01               PIC  N(04).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL4-02               PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL4-03               PIC  N(03).
       05  LBL4-04               PIC  N(01).
       05  LBL4-05               PIC  N(03).
       05  FILLER                PIC  X(04)  VALUE  SPACE.
*
 01  LBL5                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL5-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL5-01               PIC  N(04).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL5-02               PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL5-03               PIC  N(07).
       05  FILLER                PIC  X(04)  VALUE  SPACE.
*
 01  LBL6                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL6-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL6-01               PIC  N(03).
       05  LBL6-02               PIC  X(03).
       05  LBL6-03               PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL6-04               PIC  X(15).
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*
 01  LBL7                        CHARACTER  TYPE  IS  CHR-2.
     03  LBL7-A                  OCCURS     4.
       05  FILLER                PIC  X(05)  VALUE  SPACE.
       05  LBL7-01               PIC  N(03).
       05  LBL7-02               PIC  X(03).
       05  LBL7-03               PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  LBL7-04               PIC  X(15).
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-STTEN             PIC   9(05).
 01  PARA-BUMON             PIC   9(03).
 01  PARA-KOGUTI            PIC   9(03).
****************************************************************
 PROCEDURE              DIVISION USING PARA-TOKCD
                                       PARA-STTEN
                                       PARA-BUMON
                                       PARA-KOGUTI.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*店舗マスタ
 TENERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY2110L TEN ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY2110L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN
              UNTIL     END-FLG   =  "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NSY2110L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     TENMS1.
     OPEN     OUTPUT    PRTFILE.
*クリア
     MOVE     ZERO           TO   WK-CNT.
     INITIALIZE    WK-CNT  FLAGS.
     MOVE     SPACE          TO   LBL1 LBL2 LBL3 LBL4 LBL5 LBL6
                                  LBL7.
*店舗マスタ索引
     MOVE     PARA-TOKCD     TO   TEN-F52.
     MOVE     PARA-STTEN     TO   TEN-F011
     PERFORM  900-TEN-READ.
*存在判断
     IF   HTENMS-INV-FLG = "INV"
          DISPLAY NC"＃店舗ＣＤ未登録！！" UPON CONS
          STOP  RUN
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*読込数・総読み込み枚数を加算。
     ADD     1          TO       IX.
     ADD     1          TO       WK-KOSU.
*項目セット
*  店舗名称
     MOVE    TEN-F02        TO         LBL1-01(IX).
*特注一括センター
     MOVE NC"特注一括センター" TO      LBL2-01(IX).
*部門ＣＤ
*    部門
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     1              TO        ZERO-FLG.
     MOVE     PARA-BUMON     TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE   NC"部門番号"     TO        LBL3-01(IX).
     MOVE   NC"："           TO        LBL3-02(IX).
     MOVE     WK-HENKAN-N    TO        LBL3-031(IX).
     MOVE   NC"＃"           TO        LBL3-03(IX).
*    個口数（分子）
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     ZERO           TO        ZERO-FLG.
     MOVE     WK-KOSU        TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N    TO        LBL4-03(IX).
*    個口数（分母）
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     ZERO           TO        ZERO-FLG.
     MOVE     PARA-KOGUTI    TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N    TO        LBL4-05(IX).
     MOVE   NC"　個口数"     TO        LBL4-01(IX).
     MOVE   NC"："           TO        LBL4-02(IX).
     MOVE   NC"／"           TO        LBL4-04(IX).
*仕入先名
     MOVE NC"仕入先名"       TO        LBL5-01(IX).
     MOVE NC"："             TO        LBL5-02(IX).
     MOVE NC"_サカタのタネ" TO        LBL5-03(IX).
*連絡先ＴＥＬ
     MOVE NC"連絡先"         TO        LBL6-01(IX).
     MOVE   "TEL"            TO        LBL6-02(IX).
     MOVE NC"："             TO        LBL6-03(IX).
     MOVE   "045-945-8816"   TO        LBL6-04(IX).
*連絡先ＦＡＸ
     MOVE NC"連絡先"         TO        LBL7-01(IX).
     MOVE   "FAX"            TO        LBL7-02(IX).
     MOVE NC"："             TO        LBL7-03(IX).
     MOVE   "045-945-8817"   TO        LBL7-04(IX).
*
*テーブル内の枚数が４かチェック
     IF            IX                  =    4
         PERFORM   INJI-SEC
         MOVE      ZERO                TO   IX
         MOVE      SPACE               TO   LBL1 LBL2 LBL3 LBL4
                                            LBL5 LBL6 LBL7
     END-IF.
*パラメタの個口数分ラベルを印字したかチェック　　　　　
     IF            WK-KOSU             =    PARA-KOGUTI
         PERFORM   INJI-SEC
         MOVE      "END"                TO  END-FLG
     END-IF.
*
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE                   TENMS1    PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7982L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  プリントファイルへ印字                      *
*--------------------------------------------------------------*
 INJI-SEC            SECTION.
*
     WRITE    PRT-REC        FROM      LBL1      AFTER  2.
     WRITE    PRT-REC        FROM      LBL2      AFTER  1.
     WRITE    PRT-REC        FROM      LBL3      AFTER  2.
     WRITE    PRT-REC        FROM      LBL4      AFTER  1.
     WRITE    PRT-REC        FROM      LBL5      AFTER  1.
     WRITE    PRT-REC        FROM      LBL6      AFTER  1.
     WRITE    PRT-REC        FROM      LBL7      AFTER  1.
*改行
     MOVE     SPACE          TO        PRT-REC.
     WRITE    PRT-REC        AFTER  1.
*
 EXIT-INJI-SEC.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　 READ                          *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*
     READ     TENMS1
         INVALID
              MOVE      "INV"     TO   HTENMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理　　　　　                構造■2.1.1       *
*============================================================*
 KANJI-HENKAN-SEC      SECTION.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    WK-HENKAN(IZ:1)           TO     IXA.
     IF      IXA       =     ZERO
             MOVE      10              TO     IXA
     END-IF.
     EVALUATE     IZ
         WHEN     1
             IF   IXA        =    10
                  IF    ZERO-FLG  =    1
                        MOVE SU(IXA)   TO   WK-HENKAN-1
                  ELSE
                        MOVE SPACE     TO   WK-HENKAN-1
                  END-IF
             ELSE
                  MOVE       SU(IXA)   TO     WK-HENKAN-1
                  MOVE       1         TO     ZERO-FLG
             END-IF
         WHEN     2
             IF   IXA        =    10
                  IF    ZERO-FLG  =    1
                        MOVE SU(IXA)   TO   WK-HENKAN-2
                  ELSE
                        MOVE SPACE     TO   WK-HENKAN-2
                  END-IF
             ELSE
                  MOVE  SU(IXA)        TO   WK-HENKAN-2
             END-IF
         WHEN     3
             MOVE      SU(IXA)         TO     WK-HENKAN-3
     END-EVALUATE.

 KANJI-HENKAN-EXIT.
     EXIT.

```
