# SSE8704B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE8704B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　07/05/22                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
*    作成日／更新日　　　：　10/02/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭＪＡＰＡＮ商流統合による変更*
*    作成日／更新日　　　：　10/05/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　末日取得方法の変更　　　　　　　　*
*    作成日／更新日　　　：　11/02/22                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　企業名を変更（DCMﾎｰﾙﾃﾞｨﾝｸﾞｽ)      *
*    更新日／更新者　　　：　11/10/06 / YOSHIDA.M              *
*    更新概要　　　　　　：　基幹サーバ統合                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE8704B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/22.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SETGK87   ASSIGN         DA-01-VI-SETGK872
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F04
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TEN-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        STATUS         JYO-ST.
*----<< 配信データ >>--*
     SELECT   SNDSEIKY  ASSIGN         DA-01-S-SNDSEIKY
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SND-ST.
*----<< 件数データ >>--*
     SELECT   HACKENDT  ASSIGN         DA-01-S-HACKENDT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEN-ST.
*----<< 伝票データ >>-*
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SETGK87            LABEL RECORD   IS   STANDARD.
     COPY     SETGK87   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 配信データ >>--*
 FD  SNDSEIKY              LABEL     RECORD   IS   STANDARD.
 01  SND-REC.
*2010/02/02 ﾚｺｰﾄﾞ長変更
*****03  FILLER                   PIC  X(1028).
     03  FILLER                   PIC  X(1026).
*
****<< 発注集計データ >>***********************************
 FD  HACKENDT
              BLOCK CONTAINS  40  RECORDS.
     COPY     HACKENDT OF        XFDLIB
              JOINING   KEN       PREFIX.
*----<< 伝票データ >>-*
 FD  SHTDENF            BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     SHTDENF   OF   XFDLIB    JOINING   DEN  AS   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*ヘッダ情報格納領域
     COPY   DJSEIHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細　情報格納領域
     COPY   DJSEIMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*テイル情報格納領域
     COPY   DJSEITAL OF XFDLIB  JOINING   TAL  AS   PREFIX.
 01  FLAGS.
     03  END-FLG           PIC  X(03)    VALUE  SPACE.
     03  HJYOKEN-INV-FLG   PIC  X(03)    VALUE  SPACE.
     03  SHTDENF-INV-FLG   PIC  X(03)    VALUE  SPACE.
     03  HEAD-FLG          PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  RD-CNT            PIC  9(06).
     03  HED-CNT           PIC  9(06).
     03  MEI-CNT           PIC  9(06).
     03  TAL-CNT           PIC  9(06).
     03  DEN-CNT           PIC  9(06).
 01  WK-GOKEI              PIC S9(10)   VALUE  ZERO.
 01  WK-SYOHIZEI           PIC S9(10)   VALUE  ZERO.
 01  WK-SIMEBI-JYO         PIC  9(08)   VALUE  ZERO.
 01  KETA-CNT              PIC  9(02)   VALUE  ZERO.
 01  I                     PIC  9(01)   VALUE  ZERO.
 01  CHK-FLG               PIC  9(01)   VALUE  ZERO.
 01  WK-INDEX              PIC  9(02)   VALUE  ZERO.
 01  WK-STR                PIC  X(01)   VALUE  SPACE.
*伝票番号変換（文字⇒文字）
 01  WK-DENNOX             PIC  X(09).
 01  WK-DENNOX-R           REDEFINES  WK-DENNOX.
     03  WK-HEN-DENNOX     OCCURS 9.
       05  WK-HEN-DENNOXX  PIC  X(01).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO              PIC  X(09).
 01  WK-DENNO-R            REDEFINES  WK-DENNO.
     03  WK-HEN-DENNO      PIC  9(09).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO1             PIC  X(09).
 01  WK-DENNO1-R           REDEFINES  WK-DENNO1.
     03  WK-HEN-DENNO1     PIC  X(03).
     03  WK-HEN-DENNO2     PIC  X(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST                PIC  X(02).
 01  TEN-ST                PIC  X(02).
 01  JYO-ST                PIC  X(02).
 01  SND-ST                PIC  X(02).
 01  KEN-ST                PIC  X(02).
 01  DEN-ST                PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN           PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN           PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD         PIC  9(08)     VALUE  ZERO.
     03  WK-ZEI            PIC  9(01)V9(02) VALUE  ZERO.
     03  WK-SEIKYUNO       PIC  9(09)     VALUE  ZERO.
     03  WK-SEIKYUNOS      PIC  9(09)     VALUE  ZERO.
     03  WK-SEIKYUNOE      PIC  9(09)     VALUE  ZERO.
     03  WK-SIMEBI         PIC  9(08)     VALUE  ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE              PIC  9(06).
 01  FILLER                REDEFINES      SYS-DATE.
     03  SYS-YY            PIC  9(02).
     03  SYS-MM            PIC  9(02).
     03  SYS-DD            PIC  9(02).
 01  SYS-TIME              PIC  9(08).
 01  FILLER                REDEFINES      SYS-TIME.
     03  SYS-HH            PIC  9(02).
     03  SYS-MN            PIC  9(02).
     03  SYS-SS            PIC  9(02).
     03  SYS-MS            PIC  9(02).
*末日取得サブルーチン
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT          PIC 9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SETGK87-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK87.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B SETGK87 ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 SNDSEIKY-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDSEIKY.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B SNDSEIKY  ERROR " SND-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 件数データ >>--*
 HACKENDT-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACKENDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B HACKENDT  ERROR " KEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 売上データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8704B SHTDENL1  ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN  UNTIL  END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8704B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SETGK87.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    SNDSEIKY.
     OPEN     OUTPUT    HACKENDT.
     OPEN     INPUT     SHTDENF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*
     OPEN  INPUT  HJYOKEN.
*条件Ｆ（消費税率）
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=99 ZEI"  UPON STAT
              STOP  RUN
     ELSE
              MOVE  JYO-F04  TO   WK-ZEI
     END-IF.
*条件Ｆ（締日）
     MOVE     "59"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=59    "  UPON STAT
              STOP  RUN
     ELSE
              MOVE  JYO-F04  TO   WK-SIMEBI-JYO SSKTLSTD-DATE
                                  SIME-DATE
              IF   SIME-DD   =    31
                   CALL "OSKTLSTD"     USING     SSKTLSTD-DATE
                                                 SSKTLSTD-RET
              END-IF
     END-IF.
*条件Ｆ（請求書番号）
     MOVE     "34"           TO   JYO-F01.
     MOVE     "DCMJAPAN"     TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=34 DCMJAPAN"  UPON STAT
              STOP  RUN
     ELSE
              MOVE  JYO-F04   TO  WK-SEIKYUNO
              MOVE  JYO-F05   TO  WK-SEIKYUNOS
              MOVE  JYO-F06   TO  WK-SEIKYUNOE
     END-IF.
*
     CLOSE   HJYOKEN.
*
     PERFORM  SEI-READ.
*    ヘッダ行出力
     IF   END-FLG NOT = "END"
          PERFORM HEAD-WT-SEC
          MOVE   ZERO             TO   WK-GOKEI   DEN-CNT
          MOVE   SEI-F01          TO   WK-TORICD
          MOVE   1                TO   HEAD-FLG
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    明細行出力
     PERFORM MEISAI-WT-SEC
*
     PERFORM  SEI-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     IF  DEN-CNT   >  ZERO
         PERFORM  TAIL-WT-SEC
     END-IF.
*請求番号再更新
     OPEN  I-O    HJYOKEN.
     MOVE     "34"           TO   JYO-F01.
     MOVE     "DCMJAPAN"     TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG = "INV"
              DISPLAY   "HJYOKEN INV KEY=34 DCMJAPAN" UPON STAT
              STOP  RUN
     ELSE
              MOVE   WK-SEIKYUNO  TO  JYO-F04
              REWRITE  JYO-REC
     END-IF.
*    ファイルクローズ
     CLOSE    SETGK87.
     CLOSE    HTENMS HJYOKEN.
     CLOSE    SNDSEIKY.
     CLOSE    HACKENDT.
     CLOSE    SHTDENF.
*
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ INPUT =" RD-CNT  " +++" UPON CONS.
     DISPLAY "+++ HEDﾃﾞｰﾀ ｹﾝｽｳ    =" HED-CNT " +++" UPON CONS.
     DISPLAY "+++ MEIﾃﾞｰﾀ ｹﾝｽｳ    =" MEI-CNT " +++" UPON CONS.
     DISPLAY "+++ TALﾃﾞｰﾀ ｹﾝｽｳ    =" TAL-CNT " +++" UPON CONS.
*##内部統制対応 2008/08/28 NAV
     COMPUTE PARA-OUT-CNT = HED-CNT + MEI-CNT + TAL-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8704B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SETGK87   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SEI-READ-EXIT
              NOT  AT  END
              ADD       1         TO   RD-CNT
     END-READ.
*
     IF   RD-CNT(4:3) = "000" OR "500"
          DISPLAY "RD-CNT = " RD-CNT UPON CONS
     END-IF.
*
 SEI-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
****************************************************************
 JYO-RD-RTN           SECTION.
*
     READ     HJYOKEN
        INVALID      MOVE      "INV"     TO   HJYOKEN-INV-FLG
        NOT  INVALID MOVE      SPACE     TO   HJYOKEN-INV-FLG
     END-READ.
*
 JYO-RD-EXIT.
     EXIT.
****************************************************************
*　　　　ヘッダー出力
****************************************************************
 HEAD-WT-SEC            SECTION.
*    初期化
     MOVE     SPACE        TO     HED-REC.
     INITIALIZE                   HED-REC.
*    タグ
     MOVE "HD"             TO     HED-F01.
*    メッセージＩＤ
     MOVE "REMADV"         TO     HED-F02.
*    請求書番号
     ADD   1               TO     WK-SEIKYUNO.
     IF    WK-SEIKYUNO  >  WK-SEIKYUNOE
           MOVE WK-SEIKYUNOS TO WK-SEIKYUNO
     END-IF.
     MOVE  WK-SEIKYUNO     TO     HED-F03.
*    請求締日
     MOVE  "3"             TO     LINK-IN-KBN.
     MOVE  SEI-F02         TO     LINK-IN-YMD6.
     MOVE  ZERO            TO     LINK-IN-YMD8.
     MOVE  ZERO            TO     LINK-OUT-RET.
     MOVE  ZERO            TO     LINK-OUT-YMD.
     CALL  "SKYDTCKB"      USING  LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
*****MOVE  LINK-OUT-YMD    TO     HED-F04  WK-SIMEBI.
     MOVE  WK-SIMEBI-JYO   TO     HED-F04  WK-SIMEBI.
     MOVE  SSKTLSTD-DATE   TO     HED-F04  WK-SIMEBI.
*    請求区分
     MOVE  01              TO     HED-F05.
*    発注企業名称カナ
*****MOVE "DJｸﾞﾙｰﾌﾟ"       TO     HED-F06.
     MOVE "DCMﾎｰﾙﾃﾞｨﾝｸﾞｽ"
                           TO     HED-F06.
*    発注企業名称漢字
     MOVE X"28"            TO     HED-A041.
*****MOVE NC"ＤＪグループ" TO     HED-F07.
     MOVE NC"ＤＣＭホールディングス"
                           TO     HED-F07.
     MOVE X"29"            TO     HED-A042.
*    発注企業コード
*****MOVE "DJ"             TO     HED-F08.
     MOVE "DCM"            TO     HED-F08.
*    ＧＬＮ発注企業コード
     MOVE 4560181050000    TO     HED-F09.
*    請求先事業部コード
*### EVALUATE  SEI-F01
*#       WHEN  883  WHEN  14273  MOVE  02   TO  HED-F10
*#       WHEN  882  WHEN  14272  MOVE  03   TO  HED-F10
*#       WHEN  880  WHEN  1427   MOVE  04   TO  HED-F10
*#       WHEN  OTHER             MOVE SPACE TO  HED-F10
*### END-EVALUATE.
*    請求先事業部名カナ
*### EVALUATE  SEI-F01
*#       WHEN  883  WHEN  14273
*#       MOVE  "ﾎｯｶｲﾄﾞｳ ｼﾞｷﾞｮｳﾌﾞ"           TO  HED-F11
*#       WHEN  882  WHEN  14272
*#       MOVE  "ﾄｳﾎｸ ｼﾞｷﾞｮｳﾌﾞ "             TO  HED-F11
*#       WHEN  880  WHEN  1427
*#       MOVE  "ｶﾝﾄｳ ｼﾞｷﾞｮｳﾌﾞ"              TO  HED-F11
*#       WHEN  OTHER
*#       MOVE  SPACE                        TO  HED-F11
*### END-EVALUATE.
*    請求先事業部名漢字
*### MOVE X"28"         TO     HED-A081.
*#   EVALUATE  SEI-F01
*#       WHEN  883  WHEN  14273
*#       MOVE  NC"北海道事業部"             TO  HED-F12
*#       WHEN  882  WHEN  14272
*#       MOVE  NC"東北事業部"               TO  HED-F12
*#       WHEN  880  WHEN  1427
*#       MOVE  NC"関東事業部"               TO  HED-F12
*#       WHEN  OTHER
*#       MOVE  NC"　　　　　　　　　　　　　　　" TO  HED-F12
*#   END-EVALUATE.
*### MOVE X"29"         TO     HED-A082.
*    取引先コード
     EVALUATE  SEI-F01
*********WHEN  883   MOVE  880   TO  HED-F13
*********WHEN  882   MOVE  880   TO  HED-F13
*********WHEN  880   MOVE  880   TO  HED-F13
*********WHEN  14273 MOVE  1427  TO  HED-F13
*********WHEN  14272 MOVE  1427  TO  HED-F13
*********WHEN  1427  MOVE  1427  TO  HED-F13
*********WHEN  OTHER MOVE SEI-F01 TO HED-F13
         WHEN  883   WHEN  882  WHEN  880  WHEN 13938 WHEN 100403
         WHEN  100404  WHEN 100441  WHEN  100442
               MOVE  56    TO   HED-F13
         WHEN  1427  WHEN 14272 WHEN 14273 WHEN 17137  WHEN 100427
         WHEN  100428
               MOVE  250   TO   HED-F13
     END-EVALUATE.
*    取引先制御コード
     MOVE 980000        TO     HED-F14.
*### EVALUATE  SEI-F01
*#       WHEN  883   WHEN 14273
*#             MOVE  30200 TO   HED-F14
*#       WHEN  882   WHEN 14272
*#             MOVE  30300 TO   HED-F14
*#       WHEN  880   WHEN 1427
*#             MOVE  30400 TO   HED-F14
*#       WHEN  13938 WHEN 17137
*#             MOVE  10000 TO   HED-F14
*#       WHEN  OTHER
*#             MOVE  ZERO  TO   HED-F14
*### END-EVALUATE.
*    取引先名カナ
     MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"                   TO HED-F15.
*### EVALUATE  SEI-F01
*#       WHEN  883  WHEN  14273
*#       MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾎｯｶｲﾄﾞｳｼﾃﾝ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"    TO HED-F15
*#       WHEN  882  WHEN  14272
*#       MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ｾﾝﾀﾞｲｴｲｷﾞｮｳｼｮ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ" TO HED-F15
*#       WHEN  880  WHEN  1427
*#       MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾋｶﾞｼﾆﾎﾝｼﾃﾝ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"    TO HED-F15
*#       WHEN  OTHER
*#       MOVE  "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ ﾆｼﾆﾎﾝｼﾃﾝ ﾎｰﾑｶﾞｰﾃﾞﾝﾌﾞ"      TO HED-F15
*### END-EVALUATE.
*    取引先名漢字
     MOVE X"28"         TO     HED-A101.
     MOVE  NC"_サカタのタネ　ホームガーデン部" TO HED-F16.
*### EVALUATE  SEI-F01
*#       WHEN  883  WHEN  14273
*#       MOVE  NC"_サカタのタネ　北海道支店"  TO HED-F16
*#       WHEN  882  WHEN  14272
*#       MOVE  NC"_サカタのタネ　仙台営業所"  TO HED-F16
*#       WHEN  880  WHEN  1427
*#       MOVE  NC"_サカタのタネ　関東事業部"  TO HED-F16
*#       WHEN  OTHER
*#       MOVE  NC"_サカタのタネ　西日本支店"  TO HED-F16
*### END-EVALUATE.
     MOVE X"29"         TO     HED-A102.
*    個別取引先コード
*### EVALUATE  SEI-F01
*#       WHEN  883     MOVE 000880   TO   HED-F17
*#       WHEN  882     MOVE 000880   TO   HED-F17
*#       WHEN  880     MOVE 000880   TO   HED-F17
*#       WHEN  14273   MOVE 001427   TO   HED-F17
*#       WHEN  14272   MOVE 001427   TO   HED-F17
*#       WHEN  1427    MOVE 001427   TO   HED-F17
*#       WHEN  13938   MOVE 013938   TO   HED-F17
*#       WHEN  17137   MOVE 017137   TO   HED-F17
*###     WHEN  100403  MOVE 100403   TO   HED-F17
*    企業コード
     MOVE  "98"                      TO   HED-F18.
*### EVALUATE  SEI-F01
*#       WHEN  883    WHEN  14273
*#       WHEN  882    WHEN  14272
*#       WHEN  880    WHEN  1427
*#       MOVE  "03"                   TO  HED-F18
*#       WHEN  13938  WHEN  17137
*#       MOVE  "01"                   TO  HED-F18
*#       WHEN  100403
*#       MOVE  "02"                   TO  HED-F18
*### END-EVALUATE.
*****DISPLAY " HED-F18 = " HED-F18 UPON CONS.
*    企業名称（カナ）
*****MOVE  "DCM JAPAN"                TO  HED-F19.
     MOVE  "DCMﾎｰﾙﾃﾞｨﾝｸﾞｽ"
                                      TO  HED-F19.
*### EVALUATE  SEI-F01
*#       WHEN  883    WHEN  14273
*#       WHEN  882    WHEN  14272
*#       WHEN  880    WHEN  1427
*#         MOVE  "ﾎｰﾏｯｸ"              TO  HED-F19
*#       WHEN  13938  WHEN  17137
*#         MOVE  "ｶｰﾏ"                TO  HED-F19
*#       WHEN  100403
*#         MOVE  "ﾀﾞｲｷ"               TO  HED-F19
*### END-EVALUATE.
*    企業名称（漢字）
     MOVE X"28"                 TO  HED-A141.
*****MOVE NC"ＤＣＭ　Ｊａｐａｎ"      TO  HED-F20.
     MOVE NC"ＤＣＭホールディングス"
                                      TO  HED-F20.
*### EVALUATE  SEI-F01
*#       WHEN  883    WHEN  14273
*#       WHEN  882    WHEN  14272
*#       WHEN  880    WHEN  1427
*#         MOVE  NC"ホーマック"       TO  HED-F20
*#       WHEN  13938  WHEN  17137
*#         MOVE  NC"カーマ"           TO  HED-F20
*#       WHEN  100403
*#         MOVE  NC"ダイキ"           TO  HED-F20
*### END-EVALUATE.
     MOVE X"29"         TO     HED-A142.
     MOVE "0"           TO     HED-A15(149:1).
*
     MOVE   SPACE       TO     SND-REC.
*****INITIALIZE                SND-REC.
     MOVE   HED-REC     TO     SND-REC.
     WRITE  SND-REC.
*
     ADD    1           TO     HED-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*　　　　明細出力
****************************************************************
 MEISAI-WT-SEC          SECTION.
*    初期化
     MOVE     SPACE        TO     MEI-REC.
     INITIALIZE                   MEI-REC.
*    タグ
     MOVE "DT"             TO     MEI-F01.
*    発注伝票番号
     MOVE  SEI-F05         TO     MEI-F02.
*### 2010/02/02 ダイキも追加する。
     IF    SEI-F01 = 13938  OR  SEI-F01 = 17137
     OR    SEI-F01 = 100403 OR  SEI-F01 = 100404
     OR    SEI-F01 = 100441 OR  SEI-F01 = 100442
     OR    SEI-F01 = 100427 OR  SEI-F01 = 100428
           MOVE    "000"   TO     MEI-F02(1:3)
     END-IF.
     IF    SEI-F01 = 880   OR SEI-F01 = 1427
     OR    SEI-F01 = 882   OR SEI-F01 = 14272
     OR    SEI-F01 = 883   OR SEI-F01 = 14273
           MOVE      SPACE       TO   WK-DENNOX
           MOVE      ZERO        TO   WK-DENNO
           MOVE      0           TO   KETA-CNT
           MOVE      0           TO   CHK-FLG
           MOVE      MEI-F02     TO   WK-DENNOX
           PERFORM   VARYING  I  FROM 1  BY   1  UNTIL CHK-FLG = 1
                IF   WK-HEN-DENNOX(I)      =  ZERO
                     ADD  1 TO KETA-CNT
                ELSE
                     MOVE  1   TO  CHK-FLG
                END-IF
           END-PERFORM
         PERFORM  VARYING I FROM 1 BY 1 UNTIL I > (9 - KETA-CNT)
                     COMPUTE WK-INDEX = I + KETA-CNT
                     MOVE WK-HEN-DENNOX(WK-INDEX) TO WK-STR
                     MOVE WK-STR TO WK-HEN-DENNOX(I)
           END-PERFORM
           PERFORM  VARYING I FROM 1 BY 1 UNTIL I > KETA-CNT
                     COMPUTE WK-INDEX = 10 - I
                     MOVE SPACE           TO
                          WK-HEN-DENNOX(WK-INDEX)
           END-PERFORM
           MOVE      WK-DENNOX TO  MEI-F02
     END-IF.
*    請求額
     IF    SEI-F06  <  ZERO
           MOVE   "-"      TO     MEI-F031
     ELSE
           MOVE   "0"      TO     MEI-F031
     END-IF.
     MOVE  SEI-F06         TO     MEI-F032.
     ADD   SEI-F06         TO     WK-GOKEI.
*納品事業会社コード
     EVALUATE  SEI-F01
         WHEN  883    WHEN    14273  MOVE  03   TO  MEI-F033
         WHEN  882    WHEN    14272  MOVE  03   TO  MEI-F033
         WHEN  880    WHEN    1427   MOVE  03   TO  MEI-F033
         WHEN  13938  WHEN    17137  MOVE  01   TO  MEI-F033
         WHEN  100403 WHEN    100441 MOVE  02   TO  MEI-F033
         WHEN  100404 WHEN    100442 MOVE  02   TO  MEI-F033
         WHEN  100427 WHEN    100428 MOVE  02   TO  MEI-F033
     END-EVALUATE.
*個別取引先コード
     EVALUATE  SEI-F01
         WHEN  883    MOVE  000880 TO    MEI-F034
         WHEN  882    MOVE  000880 TO    MEI-F034
         WHEN  880    MOVE  000880 TO    MEI-F034
         WHEN  1427   MOVE  001427 TO    MEI-F034
         WHEN  14272  MOVE  001427 TO    MEI-F034
         WHEN  14273  MOVE  001427 TO    MEI-F034
         WHEN  13938  MOVE  013938 TO    MEI-F034
         WHEN  17137  MOVE  017137 TO    MEI-F034
         WHEN  100403 MOVE  100403 TO    MEI-F034
         WHEN  100441 MOVE  100441 TO    MEI-F034
         WHEN  100427 MOVE  100427 TO    MEI-F034
         WHEN  100404 MOVE  100403 TO    MEI-F034
         WHEN  100442 MOVE  100441 TO    MEI-F034
         WHEN  100428 MOVE  100427 TO    MEI-F034
     END-EVALUATE.
*    発注日
     MOVE  SEI-F13         TO     MEI-F04.
     IF SEI-F13  NOT  NUMERIC
           MOVE    ZERO    TO     MEI-F04
     END-IF.
     MOVE   SPACE          TO     DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE   SEI-F01        TO     DEN-F01.
     MOVE   SEI-F05        TO     DEN-F02.
     MOVE   ZERO           TO     DEN-F04.
     MOVE   SEI-F07        TO     DEN-F051.
     MOVE   1              TO     DEN-F03.
***2011.10.06 ST
     MOVE   SEI-F03        TO     DEN-F07.
     MOVE   SEI-F10        TO     DEN-F112.
***2011.10.06 EN
     READ   SHTDENF
            INVALID      MOVE "INV" TO SHTDENF-INV-FLG
            NOT  INVALID MOVE SPACE TO SHTDENF-INV-FLG
     END-READ.
     IF     SHTDENF-INV-FLG = "INV"
            MOVE   ZERO    TO     MEI-F04
     ELSE
            MOVE  DEN-F111 TO     MEI-F04
     END-IF.
*    納品日
     MOVE  "3"             TO     LINK-IN-KBN.
     MOVE  SEI-F10         TO     LINK-IN-YMD6.
     MOVE  ZERO            TO     LINK-IN-YMD8.
     MOVE  ZERO            TO     LINK-OUT-RET.
     MOVE  ZERO            TO     LINK-OUT-YMD.
     CALL  "SKYDTCKB"      USING  LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD    TO     MEI-F05  MEI-F06.
*    請求先事業部コード
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273  MOVE  02   TO  MEI-F07
         WHEN  882  WHEN  14272  MOVE  03   TO  MEI-F07
         WHEN  880  WHEN  1427   MOVE  04   TO  MEI-F07
         WHEN  OTHER             MOVE SPACE TO  MEI-F07
     END-EVALUATE.
*    納品業部名カナ
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  "ﾎｯｶｲﾄﾞｳ ｼﾞｷﾞｮｳﾌﾞ"           TO  MEI-F08
         WHEN  882  WHEN  14272
         MOVE  "ﾄｳﾎｸ ｼﾞｷﾞｮｳﾌﾞ "             TO  MEI-F08
         WHEN  880  WHEN  1427
         MOVE  "ｶﾝﾄｳ ｼﾞｷﾞｮｳﾌﾞ"              TO  MEI-F08
         WHEN  OTHER
         MOVE  SPACE                        TO  MEI-F08
     END-EVALUATE.
*    納品事業部名漢字
     MOVE X"28"         TO     MEI-A031.
     EVALUATE  SEI-F01
         WHEN  883  WHEN  14273
         MOVE  NC"北海道事業部"             TO  MEI-F09
         WHEN  882  WHEN  14272
         MOVE  NC"東北事業部"               TO  MEI-F09
         WHEN  880  WHEN  1427
         MOVE  NC"関東事業部"               TO  MEI-F09
         WHEN  OTHER
         MOVE  NC"　　　　　　　　　　　　　　　" TO MEI-F09
     END-EVALUATE.
     MOVE X"29"         TO     MEI-A032.
*    伝票種別
     EVALUATE  SEI-F07
         WHEN  40    MOVE  01    TO  MEI-F10
         WHEN  41    MOVE  02    TO  MEI-F10
         WHEN  42    MOVE  03    TO  MEI-F10
     END-EVALUATE.
*    納品先コード
     MOVE SEI-F03           TO    MEI-F11.
*    納品先名カナ
     MOVE      SEI-F01      TO    TEN-F52.
     MOVE      SEI-F03      TO    TEN-F011.
     READ   HTENMS
            INVALID
            MOVE   SPACE    TO    MEI-F12
            MOVE   X"28"    TO    MEI-A051
            MOVE ALL NC"　" TO    MEI-F13
            MOVE   X"29"    TO    MEI-A052
            NOT  INVALID
            MOVE   TEN-F04  TO    MEI-F12
            MOVE   X"28"    TO    MEI-A051
            MOVE   TEN-F03  TO    MEI-F13
            MOVE   X"29"    TO    MEI-A052
     END-READ.
     MOVE "0"           TO     MEI-A07(6:1).
*
     MOVE   SPACE       TO     SND-REC.
*****INITIALIZE                SND-REC.
     MOVE   MEI-REC     TO     SND-REC.
     WRITE  SND-REC.
*
     ADD    1           TO     MEI-CNT  DEN-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　合計行出力
****************************************************************
 TAIL-WT-SEC            SECTION.
*    初期化
     MOVE     SPACE        TO     TAL-REC  KEN-REC.
     INITIALIZE                   TAL-REC  KEN-REC.
*    請求締日
     MOVE WK-SIMEBI        TO     KEN-F01.
*    取引先コード
     MOVE WK-TORICD        TO     KEN-F02.
*    タグ
     MOVE "TR"             TO     TAL-F01.
*    伝票枚数
     MOVE  DEN-CNT         TO     TAL-F02   KEN-F03.
*    請求合計額
     IF    WK-GOKEI  <  ZERO
           MOVE   "-"      TO     TAL-F031
     ELSE
           MOVE   "0"      TO     TAL-F031
     END-IF.
     MOVE  WK-GOKEI        TO     TAL-F032  KEN-F04.
*    消費税額計算
     COMPUTE  WK-SYOHIZEI  =  WK-GOKEI  *  ( WK-ZEI  -  1).
     IF    WK-SYOHIZEI  <  ZERO
           MOVE   "-"      TO     TAL-F041
     ELSE
           MOVE   "0"      TO     TAL-F041
     END-IF.
     MOVE  WK-SYOHIZEI     TO     TAL-F042  KEN-F05.
     MOVE "0"           TO     TAL-A04(50:1).
*
     MOVE   SPACE       TO     SND-REC.
*****INITIALIZE                SND-REC.
     MOVE   TAL-REC     TO     SND-REC.
     WRITE  SND-REC.
     WRITE  KEN-REC.
*
     ADD    1           TO     TAL-CNT.
*
 TAIL-WT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
