# SSY3910V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3910V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　オンライン作場振分情報　　　　　  *
*    　　　　　　　　　　　　ＣＳＶデータ出力　　　　　　　　  *
*    作成日／作成者　　　：　2016/08/24 INOUE                  *
*    処理概要　　　　　　：　振分リスト出力ワークより、　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　2016/09/20 INOUE                  *
*                            要望対応（ＰＳ第４版）　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3910V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/08/24.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振分ワーク店舗順
     SELECT   HURXXXWT  ASSIGN    TO        DA-01-S-HURXXXWT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    HTP-STATUS.
*振分ＣＳＶ（店舗）
     SELECT   CSVHXXXT  ASSIGN    TO        DA-01-CSVHXXXT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CTP-STATUS.
*振分ワーク商品順
     SELECT   HURXXXW   ASSIGN    TO        DA-01-S-HURXXXW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    HSM-STATUS.
*振分ＣＳＶ（明細）
     SELECT   CSVHXXXF  ASSIGN    TO        DA-01-CSVHXXXF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSM-STATUS.
*ナフコ店舗マスタ
     SELECT   NFTENMS1  ASSIGN     TO      DA-01-VI-NFTENMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     TEN-F01 TEN-F02
                        FILE    STATUS     TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*振分ワーク店舗順
******************************************************************
 FD  HURXXXWT           LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  15        RECORDS.
     COPY     HURXXXW   OF        XFDLIB
              JOINING   HTP  AS   PREFIX.
*
******************************************************************
*振分ＣＳＶ（店舗）
******************************************************************
 FD  CSVHXXXT            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   81        RECORDS.
     COPY     CSVHXXXT   OF        XFDLIB
              JOINING    CTP       PREFIX.
******************************************************************
*振分ワーク商品順
******************************************************************
 FD  HURXXXW            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  15         RECORDS.
     COPY     HURXXXW   OF        XFDLIB
              JOINING   HSM  AS   PREFIX.
*
******************************************************************
*振分ＣＳＶ（明細）
******************************************************************
 FD  CSVHXXXF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   1         RECORDS.
     COPY     CSVHXXX1   OF        XFDLIB
              JOINING    CSM       PREFIX.
******************************************************************
*ナフコ店舗マスタ
******************************************************************
 FD  NFTENMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*振分ＣＳＶ（明細）マルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CSVHXXX1  OF        XFDLIB
              JOINING   CSM1 AS   PREFIX.
* 2.項目タイトル行1
     COPY     CSVHXXX2  OF        XFDLIB
              JOINING   CSM2 AS   PREFIX.
* 3.項目タイトル行2
     COPY     CSVHXXX3  OF        XFDLIB
              JOINING   CSM3 AS   PREFIX.
* 4.明細行
     COPY     CSVHXXX4  OF        XFDLIB
              JOINING   CSM4 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  HTP-STATUS          PIC  X(02).
     03  HSM-STATUS          PIC  X(02).
     03  CTP-STATUS          PIC  X(02).
     03  CSM-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  HURXXXWT-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HURXXXW-END-FLG         PIC  X(03)     VALUE  SPACE.
 01  TENPO-HIT-FLG           PIC  X(03)     VALUE  SPACE.
 01  KEY-BRK-FLG             PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*店舗テーブル領域用
 01  TENPO-IX                PIC  9(03)     VALUE  ZERO.
 01  TENPO-TBL.
   03  TENPO-CD              PIC  9(05)     OCCURS 500.
*店舗テーブル合計数量
 01  GK-SURYO                PIC  9(10)     VALUE  ZERO.
*
*数量編集用
 01  WK-HSM-F13              PIC  9(06)     VALUE  ZERO.
 01  SUM-AREA.
  02 SUM-M161                PIC  9(06)     OCCURS 500.
*ブレイク領域
* 振分ＣＳＶ（店舗）用
 01  BR-TENPOCD              PIC  9(05)     VALUE  ZERO.
* 振分ＣＳＶ（明細）用
 01  BR-SOUKOCD              PIC  X(02)     VALUE  SPACE.
 01  BR-BUMONCD              PIC  X(04)     VALUE  SPACE.
 01  BR-SYOBUNCD             PIC  X(08)     VALUE  SPACE.
 01  BR-NOUHINBI             PIC  9(08)     VALUE  ZERO.
 01  BR-JISYASYOCD           PIC  X(16)     VALUE  SPACE.
 01  BR-AITESYOCD            PIC  X(13)     VALUE  SPACE.
 01  BR-PATNCD               PIC  X(02)     VALUE  SPACE.
*添字
 01  IX                      PIC  9(03)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3910V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3910V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3910V".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 店舗情報読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-IN2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 明細情報読込件数   = ".
         05  MSG-IN02        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT02       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-JUN                PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING LINK-JUN.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HURXXXWT.
     MOVE      "HURXXXWT"   TO   AB-FILE.
     MOVE      HTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CSVHXXXT.
     MOVE      "CSVHXXXT"   TO   AB-FILE.
     MOVE      CTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HURXXXW.
     MOVE      "HURXXXW"   TO   AB-FILE.
     MOVE      HSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CSVHXXXF.
     MOVE      "CSVHXXXF"   TO   AB-FILE.
     MOVE      CSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFTENMS1.
     MOVE      "NFTENMS1"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
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
*
*初期処理
     PERFORM  INIT-SEC.
*店舗ＣＳＶ出力
     PERFORM  MAIN1-SEC
              UNTIL     END-FLG   =  "END".
     MOVE     SPACE     TO        END-FLG.
*明細ＣＳＶ出力
     PERFORM  MAIN2-SEC
              UNTIL     END-FLG   =  "END".
*終了処理
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HURXXXWT  HURXXXW  NFTENMS1.
     OPEN     OUTPUT    CSVHXXXT  CSVHXXXF.
     DISPLAY  MSG-START UPON CONS.
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
*振分ワーク店舗順読込
     PERFORM HURXXXWT-READ-SEC.
*終了判定
     IF   HURXXXWT-END-FLG  =  "END"
          DISPLAY "＃＃出力対象店舗無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
          INITIALIZE              TENPO-TBL
     END-IF.
*
*振分ワーク商品順読込
     PERFORM HURXXXW-READ-SEC.
*終了判定
     IF   HURXXXW-END-FLG  =  "END"
          DISPLAY "＃＃出力対象明細無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
          MOVE    HSM-F03   TO    BR-SOUKOCD
          MOVE    HSM-F04   TO    BR-BUMONCD
          MOVE    HSM-F24   TO    BR-SYOBUNCD
          MOVE    HSM-F06   TO    BR-NOUHINBI
          MOVE    HSM-F07   TO    BR-AITESYOCD
          MOVE    HSM-F08   TO    BR-JISYASYOCD
          MOVE    HSM-F25   TO    BR-PATNCD
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　振分ワーク店舗順読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HURXXXWT-READ-SEC    SECTION.
*
     MOVE    "HURXXXWT-READ-SEC"    TO   S-NAME.
*
     READ     HURXXXWT
              AT  END
                  MOVE     "END"    TO  HURXXXWT-END-FLG
                  GO                TO  HURXXXWT-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 HURXXXWT-READ-EXIT.
     EXIT.
****************************************************************
*　　振分ワーク商品順読込　　　　　　　　　　　　　　　　　*
****************************************************************
 HURXXXW-READ-SEC    SECTION.
*
     MOVE    "HURXXXW-READ-SEC"    TO   S-NAME.
*
     READ     HURXXXW
              AT  END
                  MOVE     "END"    TO  HURXXXW-END-FLG
                  GO                TO  HURXXXW-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT2.
*
 HURXXXW-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"           TO   S-NAME.
*
 MAIN1-01.
     MOVE     SPACE          TO      CTP-REC.
*****INITIALIZE                      CTP-REC.
*
 MAIN1-02.
     IF     ( HURXXXWT-END-FLG = "END" ) OR
            ( TENPO-IX    > 500        )
              MOVE    "END"       TO   END-FLG
              GO                  TO   MAIN1-EXIT
     ELSE
              IF   HTP-F02  NOT = BR-TENPOCD
*                振分ＣＳＶ（店舗）SET
                   PERFORM  CSVHXXXT-SET-SEC
*                振分ＣＳＶ（店舗）OUT
                   WRITE    CTP-REC
                   ADD      1           TO  OUT-CNT
*                ブレイクキー入替
                   MOVE     HTP-F02     TO  BR-TENPOCD
*                店舗テーブル添字アップ
                   ADD      1           TO  TENPO-IX
*                店舗テーブルセット（内部テーブル）
                   MOVE     HTP-F02     TO  TENPO-CD(TENPO-IX)
              END-IF
     END-IF.
*
 MAIN1-03.
*  振分ワーク店舗順読込み
     PERFORM  HURXXXWT-READ-SEC.
*
 MAIN1-04.
     GO       TO       MAIN1-02.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶ（店舗）SET
****************************************************************
 CSVHXXXT-SET-SEC     SECTION.
*
     MOVE    "CSVHXXXT-SET-SEC"  TO  S-NAME.
*
 CSVHXXXT-SET-01.
     MOVE    HTP-F013  TO   TEN-F01.
     MOVE    HTP-F02   TO   CTP-T01  TEN-F02.
     MOVE    ","       TO   CTP-TK01.
     MOVE    ","       TO   CTP-TK02.
     MOVE    ","       TO   CTP-TK03.
     MOVE    ","       TO   CTP-TK04.
     MOVE    X"28"     TO   CTP-TS01.
     READ    NFTENMS1
        INVALID
             MOVE      NC"？？？？？？？？？？" TO CTP-T02
             MOVE      ZERO                     TO CTP-T03
             MOVE      "??"                     TO CTP-T04
        NOT  INVALID
             MOVE      TEN-F05                  TO CTP-T02
             MOVE      TEN-F12                  TO CTP-T03
             MOVE      TEN-F13                  TO CTP-T04
     END-READ.
     MOVE    X"29"     TO   CTP-TE01.
*
 CSVHXXXT-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理２　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN2-SEC     SECTION.
*
     MOVE    "MAIN2-SEC"     TO      S-NAME.
*
 MAIN2-01.
     MOVE     SPACE          TO      CSM-REC.
     MOVE     SPACE          TO      CSM1-REC.
     MOVE     SPACE          TO      CSM2-REC.
     MOVE     SPACE          TO      CSM3-REC.
     MOVE     SPACE          TO      CSM4-REC.
     INITIALIZE                      SUM-AREA.
*****INITIALIZE                      CSM-REC.
*
*  振分ＣＳＶ（明細) SET/OUT  1.帳票タイトル行
     PERFORM  CSVHXXX1-SET-SEC.
     WRITE    CSM-REC   FROM      CSM1-REC.
*
*  振分ＣＳＶ（明細) SET/OUT  2.項目タイトル行１-1
     PERFORM  CSVHXXX2-1-SET-SEC.
     WRITE    CSM-REC   FROM      CSM2-REC.
*
*↓2016/09/20追加
*  振分ＣＳＶ（明細) SET/OUT  2.項目タイトル行１-2
     PERFORM  CSVHXXX2-2-SET-SEC.
     WRITE    CSM-REC   FROM      CSM2-REC.
*
*  振分ＣＳＶ（明細) SET/OUT  2.項目タイトル行１-3
     PERFORM  CSVHXXX2-3-SET-SEC.
     WRITE    CSM-REC   FROM      CSM2-REC.
*↑2016/09/20追加
*
*  振分ＣＳＶ（明細) SET/OUT  3.項目タイトル行２
     PERFORM  CSVHXXX3-SET-SEC.
     WRITE    CSM-REC   FROM      CSM3-REC.
*
 MAIN2-02.
*
*  振分ＣＳＶ（明細) SET/OUT  4.明細行
     PERFORM  CSVHXXX4-SET-SEC.
*
*    振分ワーク商品順読込み
     PERFORM  HURXXXW-READ-SEC.
     IF       HURXXXW-END-FLG    =    "END"
              MOVE    "END"      TO    END-FLG
*           レコード出力
              WRITE    CSM-REC   FROM  CSM4-REC
*           出力件数カウント
              ADD      1         TO    OUT-CNT2
*
              GO                 TO    MAIN2-EXIT
     END-IF.
*
*    キーブレイク→レコード出力
*           相手商品コード順の場合
     IF    ( LINK-JUN      =    "1"            ) AND
           (( HSM-F03  NOT =     BR-SOUKOCD    ) OR
            ( HSM-F04  NOT =     BR-BUMONCD    ) OR
            ( HSM-F24  NOT =     BR-SYOBUNCD   ) OR
            ( HSM-F06  NOT =     BR-NOUHINBI   ) OR
            ( HSM-F07  NOT =     BR-AITESYOCD  ) OR
            ( HSM-F25  NOT =     BR-PATNCD     ))
              MOVE     "BRK"     TO    KEY-BRK-FLG
     ELSE
       IF  ( LINK-JUN      =    "2"            ) AND
           (( HSM-F03  NOT =     BR-SOUKOCD    ) OR
            ( HSM-F04  NOT =     BR-BUMONCD    ) OR
            ( HSM-F24  NOT =     BR-SYOBUNCD   ) OR
            ( HSM-F06  NOT =     BR-NOUHINBI   ) OR
            ( HSM-F08  NOT =     BR-JISYASYOCD ) OR
            ( HSM-F25  NOT =     BR-PATNCD     ))
              MOVE     "BRK"     TO    KEY-BRK-FLG
       END-IF
     END-IF.
*
     IF       KEY-BRK-FLG  =     "BRK"
*           レコード出力
              WRITE    CSM-REC   FROM  CSM4-REC
*           情報リセット
              MOVE     ZERO      TO    GK-SURYO
              MOVE     SPACE     TO    KEY-BRK-FLG
              MOVE     SPACE     TO    CSM4-REC
*↓2016/08/31
              INITIALIZE               SUM-AREA
*↑2016/08/31
*           ブレイクキー入替
              MOVE     HSM-F03   TO    BR-SOUKOCD
              MOVE     HSM-F04   TO    BR-BUMONCD
              MOVE     HSM-F24   TO    BR-SYOBUNCD
              MOVE     HSM-F06   TO    BR-NOUHINBI
              MOVE     HSM-F07   TO    BR-AITESYOCD
              MOVE     HSM-F08   TO    BR-JISYASYOCD
              MOVE     HSM-F25   TO    BR-PATNCD
*           出力件数カウント
              ADD      1         TO    OUT-CNT2
     END-IF.
*
     GO       TO       MAIN2-02.
*
 MAIN2-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶ（明細)SET   1.帳票タイトル行
****************************************************************
 CSVHXXX1-SET-SEC     SECTION.
*
     MOVE   "CSVHXXX1-SET-SEC"  TO  S-NAME.
*
 CSVHXXX1-SET-01.
*
     MOVE    SPACE                                TO CSM1-REC.
*
     MOVE    X"28"                                TO CSM1-CS00.
     MOVE    NC"オンライン作場別振分リストＣＳＶ" TO CSM1-C00.
     MOVE    X"29"                                TO CSM1-CE00.
     MOVE    ","                                  TO CSM1-CK00.
*
 CSVHXXX1-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶ（明細)SET   2.項目タイトル行１-1
****************************************************************
 CSVHXXX2-1-SET-SEC     SECTION.
*
     MOVE   "CSVHXXX2-1-SET-SEC"  TO  S-NAME.
*
 CSVHXXX2-1-SET-01.
*
     MOVE    SPACE                                TO CSM2-REC.
*
     MOVE    ALL ","                              TO CSM2-TK01.
     MOVE    X"28"                                TO CSM2-TS15.
     MOVE    NC"店舗情報　"                       TO CSM2-T15.
     MOVE    X"29"                                TO CSM2-TE15.
     MOVE    ","                                  TO CSM2-TK15.
*
 CSVHXXX2-1-SET-EXIT.
     EXIT.
****************************************************************
*↓2016/09/20追加
*振分ＣＳＶ（明細)SET   2.項目タイトル行１-2
****************************************************************
 CSVHXXX2-2-SET-SEC     SECTION.
*
     MOVE   "CSVHXXX2-2-SET-SEC"  TO  S-NAME.
*
 CSVHXXX2-2-SET-01.
*
     MOVE    SPACE                                TO CSM2-REC.
*
     MOVE    ALL ","                              TO CSM2-TK01.
     MOVE    X"28"                                TO CSM2-TS15.
     MOVE    NC"県ＣＤ　　"                       TO CSM2-T15.
     MOVE    X"29"                                TO CSM2-TE15.
     MOVE    ","                                  TO CSM2-TK15.
*
 CSVHXXX2-2-SET-EXIT.
     EXIT.
****************************************************************
*↓2016/09/20追加
*振分ＣＳＶ（明細)SET   2.項目タイトル行１-3
****************************************************************
 CSVHXXX2-3-SET-SEC     SECTION.
*
     MOVE   "CSVHXXX2-3-SET-SEC"  TO  S-NAME.
*
 CSVHXXX2-3-SET-01.
*
     MOVE    SPACE                                TO CSM2-REC.
*
     MOVE    ALL ","                              TO CSM2-TK01.
     MOVE    X"28"                                TO CSM2-TS15.
     MOVE    NC"エリアＣＤ"                       TO CSM2-T15.
     MOVE    X"29"                                TO CSM2-TE15.
     MOVE    ","                                  TO CSM2-TK15.
*
 CSVHXXX2-3-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶ（明細)SET   3.項目タイトル行２
****************************************************************
 CSVHXXX3-SET-SEC     SECTION.
*
     MOVE   "CSVHXXX3-SET-SEC"  TO  S-NAME.
*
 CSVHXXX3-SET-01.
*
     MOVE    SPACE                                TO CSM3-REC.
*
     MOVE    X"28"                                TO CSM3-KS01.
     MOVE    NC"管理番号"                         TO CSM3-K01.
     MOVE    X"29"                                TO CSM3-KE01.
     MOVE    ","                                  TO CSM3-KK01.
*
     MOVE    X"28"                                TO CSM3-KS02.
     MOVE    NC"バッチ日付"                       TO CSM3-K02.
     MOVE    X"29"                                TO CSM3-KE02.
     MOVE    ","                                  TO CSM3-KK02.
*
     MOVE    X"28"                                TO CSM3-KS03.
     MOVE    NC"バッチ時刻"                       TO CSM3-K03.
     MOVE    X"29"                                TO CSM3-KE03.
     MOVE    ","                                  TO CSM3-KK03.
*
     MOVE    X"28"                                TO CSM3-KS04.
     MOVE    NC"バッチ取引先"                     TO CSM3-K04.
     MOVE    X"29"                                TO CSM3-KE04.
     MOVE    ","                                  TO CSM3-KK04.
*
     MOVE    X"28"                                TO CSM3-KS05.
     MOVE    NC"倉庫ＣＤ"                         TO CSM3-K05.
     MOVE    X"29"                                TO CSM3-KE05.
     MOVE    ","                                  TO CSM3-KK05.
*
     MOVE    X"28"                                TO CSM3-KS06.
     MOVE    NC"納品日"                           TO CSM3-K06.
     MOVE    X"29"                                TO CSM3-KE06.
     MOVE    ","                                  TO CSM3-KK06.
*
     MOVE    X"28"                                TO CSM3-KS07.
     MOVE    NC"量販店商品ＣＤ"                   TO CSM3-K07.
     MOVE    X"29"                                TO CSM3-KE07.
     MOVE    ","                                  TO CSM3-KK07.
*
     MOVE    X"28"                                TO CSM3-KS08.
     MOVE    NC"サカタコード"                     TO CSM3-K08.
     MOVE    X"29"                                TO CSM3-KE08.
     MOVE    ","                                  TO CSM3-KK08.
*
     MOVE    X"28"                                TO CSM3-KS09.
*↓2016/09/20追加
*    MOVE    NC"品単コード"                       TO CSM3-K09.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSM3-K09.
*↑2016/09/20追加
     MOVE    X"29"                                TO CSM3-KE09.
     MOVE    ","                                  TO CSM3-KK09.
*
     MOVE    X"28"                                TO CSM3-KS10.
     MOVE    NC"商品名１"                         TO CSM3-K10.
     MOVE    X"29"                                TO CSM3-KE10.
     MOVE    ","                                  TO CSM3-KK10.
*
     MOVE    X"28"                                TO CSM3-KS11.
     MOVE    NC"商品名２"                         TO CSM3-K11.
     MOVE    X"29"                                TO CSM3-KE11.
     MOVE    ","                                  TO CSM3-KK11.
*
     MOVE    X"28"                                TO CSM3-KS12.
     MOVE    NC"原価単価"                         TO CSM3-K12.
     MOVE    X"29"                                TO CSM3-KE12.
     MOVE    ","                                  TO CSM3-KK12.
*
     MOVE    X"28"                                TO CSM3-KS13.
     MOVE    NC"売価単価"                         TO CSM3-K13.
     MOVE    X"29"                                TO CSM3-KE13.
     MOVE    ","                                  TO CSM3-KK13.
*
*↓2016/09/20追加
     MOVE    X"28"                                TO CSM3-KS18.
     MOVE    NC"入数"                             TO CSM3-K18.
     MOVE    X"29"                                TO CSM3-KE18.
     MOVE    ","                                  TO CSM3-KK18.
*↑2016/09/20追加
*
     MOVE    X"28"                                TO CSM3-KS14.
     MOVE    NC"商品分類ＣＤ"                     TO CSM3-K14.
     MOVE    X"29"                                TO CSM3-KE14.
     MOVE    ","                                  TO CSM3-KK14.
*
     MOVE    X"28"                                TO CSM3-KS15.
     MOVE    NC"パターンＣＤ"                     TO CSM3-K15.
     MOVE    X"29"                                TO CSM3-KE15.
     MOVE    ","                                  TO CSM3-KK15.
*
     MOVE    ALL ","                              TO CSM3-KK16.
*
     MOVE    X"28"                                TO CSM3-KS17.
     MOVE    NC"店舗合計"                         TO CSM3-K17.
     MOVE    X"29"                                TO CSM3-KE17.
     MOVE    ","                                  TO CSM3-KK17.
*
 CSVHXXX3-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶ（明細)SET   4.明細行
****************************************************************
 CSVHXXX4-SET-SEC     SECTION.
*
     MOVE     "CSVHXXX4-SET-SEC"  TO  S-NAME.
*
 CSVHXXX4-SET-01.
*
*管理番号
     MOVE    HSM-F23   TO   CSM4-M01.
     MOVE    ","       TO   CSM4-MK01.
*バッチ日付
     MOVE    HSM-F011  TO   CSM4-M02.
     MOVE    ","       TO   CSM4-MK02.
*バッチ時刻
     MOVE    HSM-F012  TO   CSM4-M03.
     MOVE    ","       TO   CSM4-MK03.
*バッチ取引先
     MOVE    HSM-F013  TO   CSM4-M04.
     MOVE    ","       TO   CSM4-MK04.
*倉庫ＣＤ
     MOVE    HSM-F03   TO   CSM4-M05.
     MOVE    ","       TO   CSM4-MK05.
*納品日
     MOVE    HSM-F06   TO   CSM4-M06.
     MOVE    ","       TO   CSM4-MK06.
*量販店商品ＣＤ
     MOVE    HSM-F07   TO   CSM4-M07.
     MOVE    ","       TO   CSM4-MK07.
*サカタコード
     MOVE    HSM-F081  TO   CSM4-M08.
     MOVE    ","       TO   CSM4-MK08.
*↓2016/09/20
*品単コード
*→ＪＡＮＣＤに変更
*    MOVE    HSM-F082  TO   CSM4-M09.
     MOVE    HSM-F26   TO   CSM4-M09.
*↑2016/09/20
     MOVE    ","       TO   CSM4-MK09.
*商品名１
     MOVE    HSM-F111  TO   CSM4-M10.
     MOVE    ","       TO   CSM4-MK10.
*商品名２
     MOVE    HSM-F112  TO   CSM4-M11.
     MOVE    ","       TO   CSM4-MK11.
*原価単価
     MOVE    HSM-F15   TO   CSM4-M12.
     MOVE    ","       TO   CSM4-MK12.
*売価単価
     MOVE    HSM-F16   TO   CSM4-M13.
     MOVE    ","       TO   CSM4-MK13.
*↓2016/09/20
*入数
     MOVE    HSM-F27   TO   CSM4-M18.
     MOVE    ","       TO   CSM4-MK18.
*↑2016/09/20
*商品分類ＣＤ
     MOVE    HSM-F24   TO   CSM4-M14.
     MOVE    ","       TO   CSM4-MK14.
*パターンＣＤ
     MOVE    HSM-F25   TO   CSM4-M15.
     MOVE    ","       TO   CSM4-MK15.
*店舗情報（数量）OCCURS500
     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
              IX       >    500
              MOVE    ","   TO   CSM4-MK161(IX)
     END-PERFORM.
*
     MOVE     SPACE    TO   TENPO-HIT-FLG.
     PERFORM  VARYING  IX   FROM 1 BY 1
       UNTIL  IX       > 500
*
              IF       HSM-F02 = TENPO-CD(IX)
*                    該当箇所に数量セット
*↓TEST
*                IF   HSM-F07 = "22787194     "   AND
*                     HSM-F06 = 20160727
*                     DISPLAY "商品＝" HSM-F07 UPON CONS
*                     DISPLAY "　納日＝" HSM-F06 UPON CONS
*                     DISPLAY "　店舗＝" HSM-F02 UPON CONS
*                     DISPLAY "　数量＝" HSM-F13 UPON CONS
*                     DISPLAY "　ＩＸ＝" IX      UPON CONS
*                     DISPLAY "　集前＝" SUM-M161(IX) UPON CONS
*                     DISPLAY "　店前＝" CSM4-M161(IX) UPON CONS
*                END-IF
*↑TEST
                       MOVE      HSM-F13      TO   WK-HSM-F13
*↓2016/08/31
*---                   MOVE      WK-HSM-F13   TO   CSM4-M161(IX)
                       ADD       WK-HSM-F13   TO   SUM-M161(IX)
                       MOVE      SUM-M161(IX) TO   CSM4-M161(IX)
*↓TEST
*                IF   HSM-F07 = "22787194     "   AND
*                     HSM-F06 = 20160727
*                     DISPLAY "→集後＝" SUM-M161(IX) UPON CONS
*                     DISPLAY "→店後＝" CSM4-M161(IX) UPON CONS
*                END-IF
*↑TEST
*↑2016/08/31
*                    １レコード分の数量合計に加算
                       ADD       HSM-F13   TO   GK-SURYO
*
                       MOVE      "HIT"     TO   TENPO-HIT-FLG
*
              END-IF
     END-PERFORM.
*
     IF       TENPO-HIT-FLG  NOT = "HIT"
              DISPLAY  NC"内部テーブルに該当店舗なし！？"
                                                     UPON CONS
              DISPLAY  NC"店舗コード＝" HSM-F02      UPON CONS
              MOVE     4010      TO     PROGRAM-STATUS
              STOP     RUN
     END-IF.
*
*店舗合計（１レコード分の数量合計）
     MOVE    GK-SURYO  TO   CSM4-M17.
     MOVE    ","       TO   CSM4-MK17.
*
*↓2016/09/20
*    MOVE    ","       TO   CSM4-MFIL00(48:1).
     MOVE    ","       TO   CSM4-MFIL00(36:1).
*↑2016/09/20
*
 CSVHXXX4-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     RD-CNT2    TO      MSG-IN02.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     MOVE     OUT-CNT2   TO      MSG-OUT02.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
     DISPLAY  MSG-IN2    UPON    CONS.
     DISPLAY  MSG-OUT2   UPON    CONS.
*
     CLOSE     NFTENMS1.
     CLOSE     HURXXXWT  CSVHXXXT.
     CLOSE     HURXXXW  CSVHXXXF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
