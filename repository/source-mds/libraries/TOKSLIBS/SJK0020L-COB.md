# SJK0020L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0020L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品業務　　　　　　　　　*
*    モジュール名　　　　：　出荷制限商品取込エラーＣＳＶ出力 *
*    作成日／更新日　　　：　2015/10/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷制限商品取込エラーＣＳＶ出力　*
*                        ：　を行う。　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SJK0020L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/10/08.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< 出荷制限商品ワーク           >>--*
     SELECT   SYUSGWF   ASSIGN         DA-01-VI-SYUSGWL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SGW-F10
                                       SGW-F01
                                       SGW-F02
                                       SGW-F03
                        STATUS         SGW-ST.
*----<< 店舗マスタ                   >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TEN-ST.
*----<< 取引先マスタ                 >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< ＣＳＶデータ >>--*
     SELECT   SYUSEGER  ASSIGN         DA-01-S-SYUSEGER
                        STATUS         ERR-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< 出荷制限商品ワーク           >>--*
 FD  SYUSGWF     LABEL     RECORD    IS        STANDARD.
     COPY        SYUSGWF   OF        XFDLIB
     JOINING     SGW       AS        PREFIX.
*
*----<< 店舗マスタ                   >>--*
 FD  HTENMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*----<< 取引先マスタ                 >>--*
 FD  HTOKMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*----<< 出荷制限商品データ（ＣＳＶ） >>--*
 FD  SYUSEGER                                                             
                        BLOCK    CONTAINS  1    RECORDS                   
                        LABEL    RECORD    IS   STANDARD.                 
 01  SYUSEGER-REC.                                                        
     03  FILLER              PIC  X(1000).                                
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
 01  FILE-STATUS.
     03  SGW-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  ERR-ST              PIC  X(02).
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE   SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  RD-CNT              PIC  9(07)  VALUE   ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE   ZERO.
*
 01  WK-HEAD.
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(06)  VALUE   NC"取引先コード".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(04)  VALUE   NC"取引先名".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(07)  VALUE   NC"相手商品コード".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(05)  VALUE   NC"相手商品名".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(05)  VALUE   NC"店舗コード".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(03)  VALUE   NC"店舗名".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(10)  VALUE   NC"商品コード存在エラー".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(10)  VALUE   NC"店舗コード存在エラー".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(06)  VALUE   NC"マスタ作成日".
     03  FILLER     PIC  X(01)  VALUE   X"29".
     03  FILLER     PIC  X(01)  VALUE   ",".
     03  FILLER     PIC  X(01)  VALUE   X"28".
     03  FILLER     PIC  N(07)  VALUE   NC"マスタ作成時刻".
     03  FILLER     PIC  X(01)  VALUE   X"29".
*
 01  WK-MEISAI.
     03  MS-TOKCD   PIC  9(08).
     03  KANMA1     PIC  X(01).
     03  MS-TOKNM1  PIC  X(01).
     03  MS-TOKNM   PIC  N(20).
     03  MS-TOKNM2  PIC  X(01).
     03  KANMA2     PIC  X(01).
     03  MS-SYOCD   PIC  X(13).
     03  KANMA3     PIC  X(01).
     03  MS-SYONM   PIC  X(20).
     03  KANMA4     PIC  X(01).
     03  MS-TENCD   PIC  9(05).
     03  KANMA5     PIC  X(01).
     03  MS-TENNM1  PIC  X(01).
     03  MS-TENNM   PIC  N(20).
     03  MS-TENNM2  PIC  X(01).
     03  KANMA6     PIC  X(01).
     03  MS-SYOERR  PIC  X(01).
     03  KANMA7     PIC  X(01).
     03  MS-TENERR  PIC  X(01).
     03  KANMA8     PIC  X(01).
     03  MS-DATE    PIC  X(10).
     03  KANMA9     PIC  X(01).
     03  MS-TIME    PIC  X(08).
*日付時刻メッセージ表示用
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
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
*
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
*
 01  IN-DATA                 PIC  X(01).
 01  FILE-ERR.
     03  SGW-ERR             PIC  N(12) VALUE
         NC"出荷制限商品ワークエラー".
     03  TEN-ERR             PIC  N(12) VALUE
         NC"店舗マスタエラー".
     03  TOK-ERR             PIC  N(12) VALUE
         NC"取引先マスタエラー".
     03  ERR-ERR             PIC  N(12) VALUE
         NC"出荷制限商品エラーＣＳＶ".
*
****************************************************************
 PROCEDURE                 DIVISION.
****************************************************************
 DECLARATIVES.
 ERR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUSEGER.
     DISPLAY     ERR-ERR   UPON      CONS.
     DISPLAY     ERR-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SGW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUSGWF.
     DISPLAY     SGW-ERR   UPON      CONS.
     DISPLAY     SGW-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                     ＭＡＩＮ処理
****************************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     MAIN-SEC
                 UNTIL     END-FLG = "END".
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
****************************************************************
*                      Ｉ Ｎ Ｉ Ｔ
****************************************************************
 INIT-SEC                  SECTION.
*
*----<< 開始メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJK0020L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*----<< ｼｽﾃﾑﾋﾂﾞｹ ｼｭﾄｸ >>--*
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
*
*----<< ｼｽﾃﾑｼﾞｺｸ ｼｭﾄｸ >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN        INPUT     SYUSGWF  HTENMS  HTOKMS
                 OUTPUT    SYUSEGER.
*出荷制限商品ワークＳＴＡＲＴ
     MOVE      SPACE              TO   SGW-REC.
     INITIALIZE                        SGW-REC.
     MOVE      "1"                TO   SGW-F10.
     START  SYUSGWF  KEY  IS  >=  SGW-F10  SGW-F01 SGW-F02
                                  SGW-F03
            INVALID
            DISPLAY NC"＃＃出力対象データがありません！！＃＃"
                    UPON CONS
            MOVE    "END"         TO   END-FLG
            GO                    TO   INIT-EXIT
     END-START.
*----<< ワーク読込 >>--*
     PERFORM  900-SYUSGWF-READ.
*
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃出力対象データがありません！！＃＃"
     ELSE
          MOVE  WK-HEAD           TO   SYUSEGER-REC
          WRITE SYUSEGER-REC
          ADD   1                 TO   OUT-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                     ＭＡＩＮ処理
****************************************************************
 MAIN-SEC                  SECTION.
*
*    明細出力
     MOVE  SPACE           TO     WK-MEISAI.
     INITIALIZE                   WK-MEISAI.
*    初期値セット
     MOVE  ","             TO     KANMA1  KANMA2  KANMA3  KANMA4
                                  KANMA5  KANMA6  KANMA7  KANMA8
                                  KANMA9.
*    制御バイトセット
     MOVE  X"28"           TO     MS-TOKNM1  MS-TENNM1.
     MOVE  X"29"           TO     MS-TOKNM2  MS-TENNM2.
*    取引先情報セット
     MOVE  SGW-F01         TO     MS-TOKCD.
     MOVE  SGW-F01         TO     TOK-F01.
     PERFORM 900-TOK-READ.
     IF    HTOKMS-INV-FLG  =  "INV"
           MOVE ALL NC"＊" TO     MS-TOKNM
     ELSE
           MOVE TOK-F02    TO     MS-TOKNM
     END-IF.
*    相手商品ＣＤ情報セット
     MOVE  SGW-F03         TO     MS-SYOCD.
     MOVE  SGW-F04         TO     MS-SYONM.
*    店舗ＣＤ
     MOVE  SGW-F02         TO     MS-TENCD.
     MOVE  SGW-F01         TO     TEN-F52.
     MOVE  SGW-F02         TO     TEN-F011.
     PERFORM 900-TEN-READ.
     IF    HTENMS-INV-FLG  =  "INV"
           MOVE ALL NC"＊" TO     MS-TENNM
     ELSE
           MOVE TEN-F02    TO     MS-TENNM
     END-IF.
*    商品エラー区分
     MOVE  SGW-F09         TO     MS-SYOERR.
*    店舗エラー区分
     MOVE  SGW-F08         TO     MS-TENERR.
*    データ作成日
     MOVE  SGW-F05(1:4)    TO     MS-DATE(1:4).
     MOVE  "/"             TO     MS-DATE(5:1).
     MOVE  SGW-F05(5:2)    TO     MS-DATE(6:2).
     MOVE  "/"             TO     MS-DATE(8:1).
     MOVE  SGW-F05(7:2)    TO     MS-DATE(9:2).
*    データ作成時刻
     MOVE  SGW-F06(1:2)    TO     MS-TIME(1:2).
     MOVE  ":"             TO     MS-TIME(3:1).
     MOVE  SGW-F06(3:2)    TO     MS-TIME(4:2).
     MOVE  ":"             TO     MS-TIME(6:1).
     MOVE  SGW-F06(5:2)    TO     MS-TIME(7:2).
*    出力
     MOVE  WK-MEISAI       TO     SYUSEGER-REC.
     WRITE SYUSEGER-REC
     ADD   1               TO     OUT-CNT.
*----<< ワーク読込 >>--*
     PERFORM  900-SYUSGWF-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                       Ｅ Ｎ Ｄ
****************************************************************
 END-SEC                   SECTION.
*
*----<<FILE CLOSE >>--*
     CLOSE       SYUSGWF  HTENMS  HTOKMS.
     CLOSE       SYUSEGER.
*
*----<< 件数出力 >>--*
     DISPLAY  "*** INPUT  = " RD-CNT " ***" UPON CONS.
     DISPLAY  "*** OUTPUT = " OUT-CNT " ***" UPON CONS.
*
*----<< 終了メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJK0020L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    店舗マスタ読込
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*
     READ     HTENMS
              INVALID
              MOVE     "INV"           TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE           TO   HTENMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    取引先マスタ読込
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
*
     READ     HTOKMS
              INVALID
              MOVE     "INV"           TO   HTOKMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE           TO   HTOKMS-INV-FLG
     END-READ.
*
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    出荷制限商品ワーク
*--------------------------------------------------------------*
 900-SYUSGWF-READ       SECTION.
*
     READ     SYUSGWF
              AT  END
              MOVE     "END"           TO   END-FLG
              NOT  AT  END
              ADD       1              TO   RD-CNT
     END-READ.
*
 900-SYUSGWF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
